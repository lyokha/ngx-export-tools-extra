{-# LANGUAGE TemplateHaskell, RecordWildCards, BangPatterns, NumDecimals #-}
{-# LANGUAGE TupleSections, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Resolve
-- Copyright   :  (c) Alexey Radkov 2022-2024
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- DNS resolve utilities from the more extra tools collection for
-- <https://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport.Tools.Resolve (
    -- * Dynamic upstreams in Nginx
    -- $dynamicUpstreams

    -- * Exported type declarations
                                UName
                               ,SAddress
                               ,UQuery (..)
                               ,PriorityPolicy (..)
                               ,UData (..)
                               ,ServerData (..)
                               ,CollectedServerData
    -- * Exported functions
                               ,collectA
                               ,collectSRV
                               ,collectServerData
                               ) where

import           NgxExport
import           NgxExport.Tools.Combinators
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.TimeInterval

import           Network.DNS
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Client.BrReadWithTimeout
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Maybe
import           Data.Aeson
import           Data.Function
import           Data.List
import           Data.Bits
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Exception.Safe (handleAny)
import           Control.Arrow
import           Control.Monad
import           System.IO.Unsafe
import           System.Timeout

-- $dynamicUpstreams
--
-- With Nginx module
-- [nginx-upconf-module](https://github.com/lyokha/nginx-haskell-module/tree/master/examples/dynamicUpstreams),
-- it is possible to update servers inside upstreams dynamically. The module
-- requires an agent to update a bound variable with upstreams layout and also
-- signal that the variable has been altered. This module is such an agent. It
-- updates the variable with the upstreams layout in service
-- __/collectUpstreams/__ and signals about this in service callback
-- __/signalUpconf/__. Collecting upstreams encompasses DNS queries of /A/ and
-- /SRV/ records. The queries are configured independently for each managed
-- upstream. With /SRV/ queries, the module allows configuration of complex
-- hierarchies of priorities given that compound upstream containers named
-- /upstrands/ are in use (they are implemented in
-- [nginx-combined-upstreams-module](https://github.com/lyokha/nginx-combined-upstreams-module)).
--
-- Additionally, the module exports a number of functions and data types which
-- implement service /collectUpstreams/.
--
-- In the following example, we are going to extract IP addresses from an /SRV/
-- record for /_http._tcp.mycompany.com/ to inhabit upstream /utest/.
--
-- ==== File /test_tools_extra_prometheus.hs/
-- @
-- module TestToolsExtraResolve where
--
-- import NgxExport.Tools.Resolve ()
-- @
--
-- The file does not contain any significant declarations as we are going to use
-- only the exporters.
--
-- ==== File /nginx.conf/
-- @
-- user                    nginx;
-- worker_processes        4;
--
-- events {
--     worker_connections  1024;
-- }
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     upstream __/utest/__ {
--         zone utest 64k;
--         upconf_round_robin;
--         server localhost:9000;
--     }
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_resolve.so;
--
--     haskell_run_service __/simpleService_collectUpstreams/__ $hs_upstreams
--         \'Conf { upstreams =
--                     ['UData' { 'uQuery' =
--                                  'QuerySRV'
--                                      ('Name' \"_http._tcp.mycompany.com\")
--                                          ('SinglePriority' \"__/utest/__\")
--                            , 'uMaxFails' = 1
--                            , 'uFailTimeout' = 10
--                            }
--                     ]
--               , maxWait = 'Sec' 300
--               , waitOnException = 'Sec' 2
--               , responseTimeout = 'Unset'
--               }\';
--
--     haskell_service_var_ignore_empty $hs_upstreams;
--     haskell_service_var_in_shm upstreams 64k \/tmp $hs_upstreams;
--
--     haskell_service_var_update_callback __/simpleService_signalUpconf/__ $hs_upstreams
--         \'[\"http:\/\/127.0.0.1:8010__/\/upconf/__\"]\';
--
--     server {
--         listen          localhost:8010;
--         server_name     main;
--
--         location __/\/upconf/__ {
--             upconf $hs_upstreams;
--
--             allow 127.0.0.1;
--             deny  all;
--         }
--
--         location \/upstreams {
--             default_type application\/json;
--             echo $hs_upstreams;
--
--             allow 127.0.0.1;
--             deny  all;
--         }
--
--         location \/ {
--             proxy_pass http:\/\/utest;
--         }
--     }
--
--     server {
--         listen          localhost:9000;
--         server_name     backend9000;
--
--         location \/ {
--             echo_status 503;
--             echo \"Not configured\";
--         }
--     }
-- }
-- @
--
-- At the start of Nginx, upstream /utest/ contains a statically declared server
-- which reports /Not configured/, but so soon as service /collectUpstreams/
-- collects servers for the upstream in variable __/$hs_upstreams/__, and then
-- the /upconf/ module gets notified about this via callback /signalUpconf/, the
-- upstream gets inhabited by the collected servers. Notice that /signalUpconf/
-- accepts a /list/ of URLs which means that it can broadcast collected servers
-- to multiple /upconf/ endpoints listening on this or other hosts.
--
-- The upstream contents will be re-checked within the time interval of
-- /(1 or waitOnException, maxWait)/. Particularly, if an exception happens
-- during the collection of the servers, then the service will restart in
-- /waitOnException/. If there were no exceptions and the smallest value of
-- /TTL/ calculated from all collected servers does not exceed the value of
-- /maxWait/, then the service will restart in this time.
--
-- Too big response times may also cause exceptions during the collection of the
-- servers. The timeout is defined by the value of /responseTimeout/. In our
-- example, the timeout is not set.
--
-- Notice that we used /QuerySRV/ and /SinglePriority \"utest\"/. The latter
-- means that all collected servers will inhabit upstream /utest/ regardless of
-- their priority values. To distribute collected servers among a number of
-- upstreams, we can use /PriorityList/.
--
-- ==== File /nginx.conf/: collect upstreams with /PriorityList/
-- @
--     haskell_run_service __/simpleService_collectUpstreams/__ $hs_upstreams
--         \'Conf { upstreams =
--                     ['UData' { 'uQuery' =
--                                  'QuerySRV'
--                                      ('Name' \"_http._tcp.mycompany.com\")
--                                          ('PriorityList' [\"__/utest/__\", \"__/utest1/__\"])
--                            , 'uMaxFails' = 1
--                            , 'uFailTimeout' = 10
--                            }
--                     ]
--               , maxWait = 'Sec' 300
--               , waitOnException = 'Sec' 2
--               , responseTimeout = 'Unset'
--               }\';
-- @
--
-- With this configuration, servers with the highest priority will inhabit
-- upstream /utest/, while servers with lesser priorities will inhabit upstream
-- /utest1/. Upstream /utest1/ must also be managed by the /upconf/ module. The
-- priority list may contain more than two upstreams, in which case upstreams
-- at the beginning of the list will take higher priorities found in the
-- collected servers, while the last upstream will take the remainder of the
-- priorities.
--
-- Upstreams in the priority list can be put inside of an /upstrand/ to form the
-- main and the backup layers of servers.
--
-- ==== File /nginx.conf/: upstrand /utest/
-- @
--     upstream utest1 {
--         zone utest1 64k;
--         upconf_round_robin;
--         server localhost:9000;
--     }
--
--     __/upstrand utest/__ {
--         upstream utest;
--         upstream utest1;
--         order per_request;
--         next_upstream_statuses error timeout 5xx;
--         next_upstream_timeout 60s;
--     }
-- @
--
-- ==== File /nginx.conf/: location /\/upstrand/
-- @
--         location \/upstrand {
--             proxy_pass http:\/\/__/$upstrand\_utest/__;
--         }
-- @

-- | Upstream name.
type UName = Text

-- | Domain name or IP address with or without port.
type SAddress = Text

-- | DNS query model of the upstream(s).
--
-- There are 3 ways to get the list of server addresses:
--
-- - query /A/ records for a list of domain names,
-- - query an /SRV/ record for a single service name and then query /A/ records
--   for the collected list of domain names,
-- - the same as the previous, but distribute collected servers among a list of
--   upstreams according to the collected priorities.
data UQuery = QueryA [Name] UName                   -- ^ Query /A/ records
            | QuerySRV Name (PriorityPolicy UName)  -- ^ Query an /SRV/ record
            deriving Read

-- | Priority policy.
--
-- Specifies how to distribute collected items by priorities. In particular,
-- /PriorityPolicy UName/ specifies how to distribute collected servers among
-- the given upstreams.
data PriorityPolicy a = SinglePriority a  -- ^ All items go to a single element
                      | PriorityList [a]  -- ^ Distribute items by priorities
                      deriving Read

-- | Upstream configuration.
--
-- Includes DNS query model and parameters for Nginx /server/ description.
-- Values of /uMaxFails/ and /uFailTimeout/ get assigned to each collected
-- server as /max_fails/ and /fail_timeout/ respectively. The weight of an
-- individual server gets picked from the value of 'srvWeight' collected in
-- /SRV/ queries. Note that setting of parameters /max_conns/, /backup/ and
-- /down/ is not supported.
data UData = UData { uQuery       :: UQuery  -- ^ DNS query model
                   , uMaxFails    :: Int     -- ^ /max_fails/
                   , uFailTimeout :: Int     -- ^ /fail_timeout/
                   } deriving Read

data Conf = Conf { upstreams       :: [UData]
                 , maxWait         :: TimeInterval
                 , waitOnException :: TimeInterval
                 , responseTimeout :: TimeInterval
                 } deriving Read

-- | Server data.
--
-- The fields map exactly to parameters from Nginx /server/ description.
data ServerData = ServerData { sAddr        :: SAddress   -- ^ Server address
                             , sHost        :: SAddress   -- ^ Server host name
                             , sWeight      :: Maybe Int  -- ^ /weight/
                             , sMaxFails    :: Maybe Int  -- ^ /max_fails/
                             , sFailTimeout :: Maybe Int  -- ^ /fail_timeout/
                             } deriving (Show, Eq, Ord)

instance ToJSON ServerData where
    toJSON ServerData {..} =
        object $ catMaybes [ pure $ "addr"   .=      sAddr
                           , pure $ "host"   .=      sHost
                           , ("weight"       .=) <$> sWeight
                           , ("max_fails"    .=) <$> sMaxFails
                           , ("fail_timeout" .=) <$> sFailTimeout
                           ]

-- | Collected server data.
type CollectedServerData = Map UName [ServerData]

collectedServerData :: IORef (TimeInterval, CollectedServerData)
collectedServerData = unsafePerformIO $ newIORef (Unset, M.empty)
{-# NOINLINE collectedServerData #-}

httpManager :: Manager
httpManager = unsafePerformIO newTlsManager
{-# NOINLINE httpManager #-}

getResponse :: Text -> (Request -> IO (Response L.ByteString)) ->
    IO L.ByteString
getResponse url = fmap responseBody . (parseUrlThrow (T.unpack url) >>=)

getUrl :: Text -> IO L.ByteString
getUrl url = getResponse url $ flip httpLbsBrReadWithTimeout httpManager

minimumTTL :: TTL -> [TTL] -> TTL
minimumTTL lTTL [] = lTTL
minimumTTL _ ttls = minimum ttls

-- | Queries an /A/ record for the given domain name.
--
-- Returns a list of IP addresses and the minimum value of their TTLs. If the
-- list is empty, then the returned TTL value gets taken from the first
-- argument.
collectA
    :: TTL                      -- ^ Fallback TTL value
    -> Name                     -- ^ Domain name
    -> IO (TTL, [IPv4])
collectA lTTL name = do
    !srv <- queryA name
    return (minimumTTL lTTL $ map fst srv, map snd srv)

-- | Queries an /SRV/ record for the given service name.
--
-- After getting the /SRV/ record, runs 'collectA' for each collected element.
--
-- Returns a list of pairs /(Domain name, IP address)/ wrapped in an 'SRV'
-- container and the minimum value of their TTLs. If the list is empty, then
-- the returned TTL value gets taken from the first argument. Note that trailing
-- dots in the collected domain names (as in /www.mycompany.com./) get removed
-- in the returned list.
collectSRV
    :: TTL                      -- ^ Fallback TTL value
    -> Name                     -- ^ Service name
    -> IO (TTL, [SRV (Name, IPv4)])
collectSRV lTTL name = do
    !srv <- querySRV name
    !srv' <- mapConcurrently
                 ((\s@SRV {..} -> do
                     (t, is) <- collectA lTTL srvTarget
                     return (t
                            ,map (\v ->
                                     s { srvTarget =
                                           (removeTrailingDot srvTarget, v)
                                       }
                                 ) is
                            )
                  ) . snd
                 ) srv
    return (min (minimumTTL lTTL $ map fst srv)
                (minimumTTL lTTL $ map fst srv')
           ,concatMap snd srv'
           )
    where removeTrailingDot (Name v) = Name $ maybe v fst $ C8.unsnoc v

showIPv4 :: IPv4 -> String
showIPv4 (IPv4 w) =
  shows ((w `unsafeShiftR` 24) .&. 0xff) . ('.' :) .
  shows ((w `unsafeShiftR` 16) .&. 0xff) . ('.' :) .
  shows ((w `unsafeShiftR`  8) .&. 0xff) . ('.' :) $
  shows ( w                    .&. 0xff)
  ""

ipv4ToServerData :: UData -> Name -> IPv4 -> ServerData
ipv4ToServerData UData {..} (Name n) i =
    ServerData (T.pack $ show i) (T.decodeUtf8 n)
        Nothing (Just uMaxFails) (Just uFailTimeout)

srvToServerData :: UData -> SRV (Name, IPv4) -> ServerData
srvToServerData UData {..} SRV {..} =
    let (Name n, a) = srvTarget
        showAddr i p = showIPv4 i ++ ':' : show p
    in ServerData (T.pack $ showAddr a srvPort) (T.decodeUtf8 n)
          (Just $ fromIntegral srvWeight) (Just uMaxFails) (Just uFailTimeout)

-- | Collects server data for the given upstream configuration.
--
-- Returns the collected server data and the minimum value of all the collected
-- TTLs. If this TTL value, having been converted into a 'TimeInterval', is not
-- bigger than /maxWait/, then it defines in how long time service
-- /collectUpstreams/, which calls this function, will restart again.
collectServerData
    :: TTL                      -- ^ Fallback TTL value
    -> UData                    -- ^ Upstream configuration
    -> IO (TTL, CollectedServerData)
collectServerData lTTL (UData (QueryA [] _) _ _) =
    return (lTTL, M.empty)
collectServerData lTTL ud@(UData (QueryA ns u) _ _) = do
    a <- mapConcurrently (\n -> (n, ) <$> collectA lTTL n) ns
    return $
        minimum *** M.singleton u . concat $
            foldr (\(n, (t, s)) (ts, ss) ->
                      -- sort is required because resolver may rotate servers
                      -- which means that the same data may differ after every
                      -- single check; this note regards to other clauses of
                      -- this function as well
                      (t : ts, sort (map (ipv4ToServerData ud n) s) : ss)
                  ) ([], []) a
collectServerData lTTL ud@(UData (QuerySRV n (SinglePriority u)) _ _) = do
    (wt, srv) <- collectSRV lTTL n
    return (wt, M.singleton u $ sort $ map (srvToServerData ud) srv)
collectServerData lTTL (UData (QuerySRV _ (PriorityList [])) _ _) =
    return (lTTL, M.empty)
collectServerData lTTL ud@(UData (QuerySRV n (PriorityList pl)) _ _ ) = do
    (wt, srv) <- collectSRV lTTL n
    let srv' = zip (withTrail pl) $ partitionByPriority srv
    return (wt
           ,M.fromList $ map (second $ sort . map (srvToServerData ud)) srv'
           )
    where partitionByPriority =
              groupBy ((==) `on` srvPriority) . sortOn srvPriority
          withTrail = uncurry (++) . (id &&& repeat . last)

collectUpstreams :: Conf -> NgxExportService
collectUpstreams Conf {..} = const $ do
    (wt, old) <- readIORef collectedServerData
    when (wt /= Unset) $ threadDelaySec $ toSec wt
    let (lTTL, hTTL) = (toTTL waitOnException, toTTL maxWait)
    srv <- handleCollectErrors $
        mapConcurrently (withTimeout . collectServerData lTTL) upstreams
    let nwt = fromTTL $ max (TTL 1) $ min hTTL $ minimumTTL lTTL $ map fst srv
        new = mconcat $ map snd srv
    if new == old
        then do
            when (nwt /= wt) $
                modifyIORef' collectedServerData $ first $ const nwt
            return ""
        else do
            writeIORef collectedServerData (nwt, new)
            return $ encode new
    where toTTL = TTL . fromIntegral . toSec
          fromTTL (TTL ttl) = Sec $ fromIntegral ttl
          handleCollectErrors = handleAny $ \e -> do
              writeIORef collectedServerData (waitOnException, M.empty)
              throwIO e
          withTimeout act = do
              r <- timeout (toTimeout responseTimeout) act
              case r of
                  Nothing -> throwIO $
                      userError "Collection of server data was timed out"
                  Just r' -> return r'
          toTimeout Unset = -1
          toTimeout v = toSec v * 1e6

ngxExportSimpleServiceTyped 'collectUpstreams ''Conf $
    PersistentService Nothing

-- a list of fully qualified URLs such as 'http://../..' or 'https://../..'
type Upconf = [Text]

signalUpconf :: Upconf -> NgxExportService
signalUpconf = voidHandler' . mapConcurrently_ getUrl

ngxExportSimpleServiceTyped 'signalUpconf ''Upconf $
    PersistentService Nothing

