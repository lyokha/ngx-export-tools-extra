{-# LANGUAGE TemplateHaskell, RecordWildCards, BangPatterns, NumDecimals #-}
{-# LANGUAGE DeriveFoldable, TupleSections, LambdaCase, OverloadedStrings #-}

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
                               ,WeightedList (..)
                               ,NameList
                               ,PriorityPolicy (..)
                               ,UNamePriorityPolicy
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
import           Data.Ord
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
-- upstream. With both /A/ and /SRV/ queries, the module allows configuration
-- of complex hierarchies of priorities given that compound upstream containers
-- named /upstrands/ are in use (they are implemented in
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
-- upstream /utest/, while servers with the less priority will inhabit upstream
-- /utest1/. Upstream /utest1/ must also be managed by the /upconf/ module.
-- Generally, given the number of upstreams in the priority list is \(N\) and
-- the number of all variations of server priorities collected in the response
-- is \(M\), and \(N\) is less than \(M\), then remaining \(M - N\) servers with
-- the lowest priorities won't be used in the upstreams at all, otherwise, if
-- \(N\) is greater than \(M\), then remaining \(N - M\) upstreams at the end of
-- the priority list will contain the same servers of the lowest priority.
--
-- Upstreams in the priority list can be put inside of an /upstrand/ to form the
-- main and the backup layers of servers.
--
-- ==== File /nginx.conf/: upstrand /utest/
-- @
--     upstream __/utest1/__ {
--         zone utest1 64k;
--         upconf_round_robin;
--         server localhost:9000;
--     }
--
--     __/upstrand utest/__ {
--         upstream __/utest/__;
--         upstream __/utest1/__;
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
-- There are 4 ways to get the list of server addresses:
--
-- - query /A/ records on a weighted list of domain names,
-- - the same as the previous, but distribute collected servers among a list of
--   upstreams according to the weights set in the name list,
-- - query an /SRV/ record on a single service name and then query /A/ records
--   on the collected list of domain names,
-- - the same as the previous, but distribute collected servers among a list of
--   upstreams according to the collected priorities.
--
-- Particularly, in /SRV/ queries priorities are taken from values of
-- 'srvPriority', the less this value the higher the priority. In /A/ queries
-- priorities are taken from v'WeightedList', the higher the weight the higher
-- the priority.
--
-- Weights of individual servers depend on both priority policy and type of the
-- query:
--
-- - /single priority, A query/: weights are taken from v'WeightedList',
-- - /priority list, A query/: no weights are specified as the values from
--   v'WeightedList' are used for the priority list parameterization,
-- - /single priority, SRV query/: no weights are specified as it is not clear
--   how to choose them correctly from the two parameters 'srvPriority' and
--   'srvWeight',
-- - /priority list, SRV query/: weights are taken from 'srvWeight'.
--
-- Names in the /QueryA/ name list may contain suffix /:port/ (a port number)
-- which is ignored in 'collectA' and only appended to values of 'sAddr'
-- collected by 'collectServerData'.
data UQuery = QueryA NameList UNamePriorityPolicy  -- ^ Query /A/ records
            | QuerySRV Name UNamePriorityPolicy    -- ^ Query an /SRV/ record
            deriving Read

-- | Weighted list.
--
-- A list of elements optionally annotated by weight values.
data WeightedList a = Singleton a               -- ^ List with a single element
                    | PlainList [a]             -- ^ Plain list without weights
                    | WeightedList [(a, Word)]  -- ^ Weighted list
                    deriving (Read, Foldable)

-- | Weighted list of domain names.
type NameList = WeightedList Name

-- | Priority policy.
--
-- Specifies how to distribute collected items by priorities. In particular,
-- /PriorityPolicy UName/ specifies how to distribute collected servers among
-- the given upstreams.
data PriorityPolicy a = SinglePriority a  -- ^ All items go to a single element
                      | PriorityList [a]  -- ^ Distribute items by priorities
                      deriving Read

-- | Priority policy of upstream names.
type UNamePriorityPolicy = PriorityPolicy UName

-- | Upstream configuration.
--
-- Includes DNS query model and parameters for Nginx /server/ description.
-- Values of /uMaxFails/ and /uFailTimeout/ get assigned to each collected
-- server as /max_fails/ and /fail_timeout/ respectively. Note that setting of
-- parameters /max_conns/, /backup/ and /down/ is not supported.
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
collectA lTTL (Name n) = do
    !srv <- queryA $ Name $ C8.takeWhile (':' /=) n
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
                 ((\s@SRV {..} ->
                     second (map $ \v -> (, v) . removeTrailingDot <$> s) <$>
                         collectA lTTL srvTarget
                  ) . snd
                 ) srv
    return (min (minimumTTL lTTL $ map fst srv)
                (minimumTTL lTTL $ map fst srv')
           ,concatMap snd srv'
           )
    where removeTrailingDot (Name v) = Name $
              maybe v (\case (v', '.') -> v'; _ -> v) $ C8.unsnoc v

showIPv4 :: IPv4 -> String
showIPv4 (IPv4 w) =
  shows ((w `unsafeShiftR` 24) .&. 0xff) . ('.' :) .
  shows ((w `unsafeShiftR` 16) .&. 0xff) . ('.' :) .
  shows ((w `unsafeShiftR`  8) .&. 0xff) . ('.' :) $
  shows ( w                    .&. 0xff)
  ""

partitionByPriority :: (Eq b, Ord b) => (a -> b) -> [a] -> [[a]]
partitionByPriority f = groupBy ((==) `on` f) . sortOn f

zipWithOtherRepeatLast :: [a] -> [b] -> [(a, b)]
zipWithOtherRepeatLast _ [] = []
zipWithOtherRepeatLast xs other = zip xs $ other ++ repeat (last other)

ipv4ToServerData :: UData -> UNamePriorityPolicy -> Name -> Maybe Word ->
    IPv4 -> ServerData
ipv4ToServerData UData {..} policy (Name n) weight a =
    let port = snd $ C8.span (':' /=) n
        showAddr i p = showIPv4 i ++ C8.unpack p
    in ServerData (T.pack $ showAddr a port) (T.decodeUtf8 n)
           (case policy of
                  SinglePriority _ -> fromIntegral <$> weight
                  PriorityList _ -> Nothing
           ) (Just uMaxFails) (Just uFailTimeout)

srvToServerData :: UData -> UNamePriorityPolicy -> SRV (Name, IPv4) ->
    ServerData
srvToServerData UData {..} policy SRV {..} =
    let (Name n, a) = srvTarget
        showAddr i p = showIPv4 i ++ ':' : show p
    in ServerData (T.pack $ showAddr a srvPort) (T.decodeUtf8 n)
          (case policy of
               SinglePriority _ -> Nothing
               PriorityList _ -> Just $ fromIntegral srvWeight
          ) (Just uMaxFails) (Just uFailTimeout)

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
collectServerData lTTL (UData (QueryA ns p) _ _)
    | null ns = return (lTTL, M.empty)
    | PriorityList [] <- p = return (lTTL, M.empty)
collectServerData lTTL ud@(UData (QueryA
                                     (WeightedList ns) p@(PriorityList pl)
                                 ) _ _
                          ) = do
    a <- mapConcurrently (\nw@(n, _) -> (nw, ) <$> collectA lTTL n) ns
    return $
        minimum ***
            M.fromList . zipWithOtherRepeatLast pl
            . map (map snd) . partitionByPriority (Down . fst) . concat $
                foldr (\((n, w), (t, s)) (ts, ss) ->
                          (t : ts
                           -- sort is required because resolver may rotate
                           -- servers which means that the same data may differ
                           -- after every single check; this note regards to
                           -- other clauses of this function as well
                          ,sort (map ((w ,) .
                                         ipv4ToServerData ud p n Nothing
                                     ) s
                                ) : ss
                          )
                      ) ([], []) a
collectServerData lTTL ud@(UData (QueryA ns p) _ _) = do
    let ns' = case ns of
                  Singleton n -> pure (n, Nothing)
                  PlainList ns'' -> map (, Nothing) ns''
                  WeightedList ns'' -> map (second Just) ns''
    a <- mapConcurrently (\nw@(n, _) -> (nw, ) <$> collectA lTTL n) ns'
    let f = case p of
                SinglePriority u -> M.singleton u
                PriorityList pl -> \v -> M.fromList $ map (, v) pl
    return $
        minimum *** f . concat $
            foldr (\((n, w), (t, s)) (ts, ss) ->
                      (t : ts, sort (map (ipv4ToServerData ud p n w) s) : ss)
                  ) ([], []) a
collectServerData lTTL ud@(UData (QuerySRV n p@(SinglePriority u)) _ _) = do
    (wt, srv) <- collectSRV lTTL n
    return (wt, M.singleton u $ sort $ map (srvToServerData ud p) srv)
collectServerData lTTL (UData (QuerySRV _ (PriorityList [])) _ _) =
    return (lTTL, M.empty)
collectServerData lTTL ud@(UData (QuerySRV n p@(PriorityList pl)) _ _ ) = do
    (wt, srv) <- collectSRV lTTL n
    let srv' = zipWithOtherRepeatLast pl $ partitionByPriority srvPriority srv
    return (wt
           ,M.fromList $ map (second $ sort . map (srvToServerData ud p)) srv'
           )

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

