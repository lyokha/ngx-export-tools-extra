{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Aggregate
-- Copyright   :  (c) Alexey Radkov 2019-2021
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- An aggregate service from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Aggregate (
    -- * The typed service exporter
    -- $aggregateServiceExporter
#ifdef SNAP_AGGREGATE_SERVER
                                  AggregateServerConf,
#endif
                                  ngxExportAggregateService
    -- * Nginx-based aggregate service
    -- $nginxBasedAggregateService

    -- * The worker-side reporter
                                 ,reportAggregate
    -- * Re-exported data constructors from /Foreign.C/
    -- | Re-exports are needed by the exporter for marshalling in foreign calls.
                                 ,Foreign.C.Types.CInt (..)
                                 ,Foreign.C.Types.CUInt (..)
                                 ) where

import           NgxExport
import           NgxExport.Tools

import           Language.Haskell.TH
import           Network.HTTP.Client
import           Foreign.C.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Int
import           Data.Time.Clock.POSIX
import           Data.Aeson
import           Data.Maybe
import           Control.Monad
import           Control.Arrow
import           Control.Exception
import           System.IO.Unsafe
import           Safe

#ifdef SNAP_AGGREGATE_SERVER
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Control.Monad.IO.Class
import           Control.Exception.Enclosed (handleAny)
import           Snap.Http.Server
import           Snap.Core
#endif


type AggregateValue a = (CTime, Map Int32 (CTime, Maybe a))
type Aggregate a = IORef (AggregateValue a)

type ReportValue a = Maybe (Int32, Maybe a)

-- $aggregateServiceExporter
--
-- An aggregate service collects custom typed data reported by worker processes
-- and sends this via HTTP when requested. This is an 'ignitionService' in terms
-- of module "NgxExport.Tools", which means that it starts upon the startup of
-- the worker process and runs until termination of the worker. Internally, an
-- aggregate service starts an HTTP server implemented via the [Snap
-- framework](http://snapframework.com/), which serves incoming requests from
-- worker processes (collecting data) as well as from the Nginx server's
-- clients (reporting collected data for administration purpose).
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_aggregate.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications \#-}
-- {-\# LANGUAGE OverloadedStrings, BangPatterns \#-}
--
-- module TestToolsExtraAggregate where
--
-- import           NgxExport
-- import           NgxExport.Tools
-- import           NgxExport.Tools.Aggregate
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy.Char8 as C8L
-- import           Data.Aeson
-- import           Data.Maybe
-- import           Data.IORef
-- import           System.IO.Unsafe
-- import           GHC.Generics
--
-- data Stats = Stats { bytesSent :: Int
--                    , requests :: Int
--                    , meanBytesSent :: Int
--                    } deriving Generic
-- instance FromJSON Stats
-- instance ToJSON Stats
--
-- stats :: IORef Stats
-- stats = unsafePerformIO $ newIORef $ Stats 0 0 0
-- {-\# NOINLINE stats \#-}
--
-- updateStats :: ByteString -> IO C8L.ByteString
-- __/updateStats/__ s = do
--     let cbs = 'readFromByteString' \@Int s
--     modifyIORef\' stats $ \\(Stats bs rs _) ->
--         let !nbs = bs + fromMaybe 0 cbs
--             !nrs = rs + 1
--             !nmbs = nbs \`div\` nrs
--         in Stats nbs nrs nmbs
--     return \"\"
-- 'NgxExport.ngxExportIOYY' \'updateStats
--
-- reportStats :: Int -> Bool -> IO C8L.ByteString
-- __/reportStats/__ = 'deferredService' $ \\port -> do
--     s <- readIORef stats
--     'reportAggregate' port (Just s) \"__/stats/__\"
--     return \"\"
-- 'ngxExportSimpleServiceTyped' \'reportStats \'\'Int $
--     'PersistentService' $ Just $ Sec 5
--
-- 'ngxExportAggregateService' \"__/stats/__\" \'\'Stats
-- @
--
-- Here, on the bottom line, aggregate service /stats/ is declared. It expects
-- from worker processes reports in JSON format with data of type /Stats/ which
-- includes the number of bytes sent so far, the number of client requests, and
-- the mean value of bytes sent per a single request. Its own configuration
-- (a TCP port and the /purge interval/) shall be defined in the Nginx
-- configuration file. The reports from worker processes are sent from a
-- 'deferredService' /reportStats/ every 5 seconds: it merely reads data
-- collected in a global IORef /stats/ and then sends this to the aggregate
-- service using 'reportAggregate'. Handler /updateStats/ updates the /stats/
-- on every run. It accepts a /ByteString/ from Nginx, then converts it to an
-- /Int/ value and interprets this as the number of bytes sent in the current
-- request. It also increments the number or requests and calculates the mean
-- value of bytes sent in all requests to this worker so far. Notice that all
-- the parts of /stats/ are evaluated /strictly/, it is important!
--
-- ==== File /nginx.conf/
-- @
-- user                    nobody;
-- worker_processes        2;
--
-- events {
--     worker_connections  1024;
-- }
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_aggregate.so;
--
--     haskell_run_service __/simpleService_aggregate_stats/__ $hs_stats
--             \'__/AggregateServerConf/__ { __/asPort/__ = 8100, __/asPurgeInterval/__ = Min 5 }\';
--
--     haskell_service_var_in_shm stats 32k \/tmp $hs_stats;
--
--     haskell_run_service __/simpleService_reportStats/__ $hs_reportStats 8100;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         haskell_run __/updateStats/__ !$hs_updateStats $bytes_sent;
--
--         location \/ {
--             echo Ok;
--         }
--     }
--
--     server {
--         listen       8020;
--         server_name  stat;
--
--         location \/ {
--             allow 127.0.0.1;
--             deny all;
--             proxy_pass http:\/\/127.0.0.1:8100\/get\/__/stats/__;
--         }
--     }
-- }
-- @
--
-- The aggregate service /stats/ must be referred from the Nginx configuration
-- file with prefix __/simpleService_aggregate_/__. Its configuration is typed,
-- the type is 'AggregateServerConf'. Though its only constructor
-- /AggregateServerConf/ is not exported from this module, the service is still
-- configurable from an Nginx configuration. Here, the aggregate service listens
-- on TCP port /8100/, and its /purge interval/ is 5 minutes. Notice that an
-- aggregate service must be /shared/ (here, variable /$hs_stats/ is declared as
-- shared with Nginx directive /haskell_service_var_in_shm/), otherwise it won't
-- even start because the internal HTTP servers on each worker process won't be
-- able to bind to the same TCP port. Inside the upper /server/ clause, handler
-- /updateStats/ runs on every client request. This handler always returns an
-- empty string in variable /$hs_updateStats/ because it is only needed for the
-- side effect of updating the /stats/. However, as soon as Nginx variable
-- handlers are /lazy/, evaluation of /$hs_updateStats/ must be forced somehow.
-- To achieve this, we used the /strict annotation/ (the /bang/ symbol) in
-- directive /haskell_run/ that enforces strict evaluation in a late request
-- processing phase, when the value of variable /$bytes_sent/ has been already
-- calculated.
--
-- Data collected by the aggregate service can be obtained in a request to the
-- virtual server listening on TCP port /8020/. It simply proxies requests to
-- the internal aggregate server with URL /\/get\/__stats__/ where __/stats/__
-- corresponds to the /name/ of the aggregate service.
--
-- ==== A simple test
-- As far as /reportStats/ is a deferred service, we won't get useful data in 5
-- seconds after Nginx start.
--
-- > $ curl 'http://127.0.0.1:8020/' | jq
-- > [
-- >   "1970-01-01T00:00:00Z",
-- >   {}
-- > ]
--
-- However, later we should get some useful data.
--
-- > $ curl 'http://127.0.0.1:8020/' | jq
-- > [
-- >   "2019-04-22T14:19:04Z",
-- >   {
-- >     "5910": [
-- >       "2019-04-22T14:19:19Z",
-- >       {
-- >         "bytesSent": 0,
-- >         "requests": 0,
-- >         "meanBytesSent": 0
-- >       }
-- >     ],
-- >     "5911": [
-- >       "2019-04-22T14:19:14Z",
-- >       {
-- >         "bytesSent": 0,
-- >         "requests": 0,
-- >         "meanBytesSent": 0
-- >       }
-- >     ]
-- >   }
-- > ]
--
-- Here we have collected stats from the two Nginx worker processes with /PIDs/
-- /5910/ and /5911/. The timestamps show when the stats was updated the last
-- time. The topmost timestamp shows the time of the latest /purge/ event. The
-- data itself have only zeros as soon we have made no request to the main
-- server so far. Let's run 100 simultaneous requests and look at the stats (it
-- should update at worst in 5 seconds after running them).
--
-- > $ for i in {1..100} ; do curl 'http://127.0.0.1:8010/' & done
--
-- Wait 5 seconds...
--
-- > $ curl 'http://127.0.0.1:8020/' | jq
-- > [
-- >   "2019-04-22T14:29:04Z",
-- >   {
-- >     "5910": [
-- >       "2019-04-22T14:31:34Z",
-- >       {
-- >         "bytesSent": 17751,
-- >         "requests": 97,
-- >         "meanBytesSent": 183
-- >       }
-- >     ],
-- >     "5911": [
-- >       "2019-04-22T14:31:31Z",
-- >       {
-- >         "bytesSent": 549,
-- >         "requests": 3,
-- >         "meanBytesSent": 183
-- >       }
-- >     ]
-- >   }
-- > ]

throwUserError :: String -> IO a
throwUserError = ioError . userError

encodeAggregate :: ToJSON a => AggregateValue a -> L.ByteString
encodeAggregate = encode . (toUTCTime *** M.map (first toUTCTime))
    where toUTCTime (CTime t) = posixSecondsToUTCTime $ fromIntegral t

receiveAggregate' :: Aggregate a -> ReportValue a -> TimeInterval -> IO ()
receiveAggregate' a s tint = do
    let (pid, v) = fromJust s
        int = fromIntegral . toSec $ tint
    !t <- ngxNow
    atomicModifyIORef' a $
        \(t', v') ->
            (let (!tn, f) =
                     if t - t' >= int
                         then (t, M.filter $ \(t'', _) -> t - t'' < int)
                         else (t', id)
                 !vn = f $ M.alter
                           (\old ->
                               let !new' =
                                       if isNothing old || isJust v
                                           then v
                                           else snd $ fromJust old
                               in Just (t, new')
                           ) pid v'
             in (tn, vn)
            ,()
            )

receiveAggregate :: FromJSON a =>
    Aggregate a -> L.ByteString -> ByteString -> IO L.ByteString
receiveAggregate a v sint = do
    let !s = decode' v
        !int = readDef (Min 5) $ C8.unpack sint
    when (isNothing s) $ throwUserError "Unreadable aggregate!"
    receiveAggregate' a (fromJust s) int
    return "done"

sendAggregate :: ToJSON a =>
    Aggregate a -> ByteString -> IO ContentHandlerResult
sendAggregate a = const $ do
    s <- readIORef a
    return (encodeAggregate s, "text/plain", 200, [])


#ifdef SNAP_AGGREGATE_SERVER

-- | Configuration of an aggregate service.
--
-- This type is exported because Template Haskell requires that. Though its
-- only constructor /AggregateServerConf/ is not exported, it is still reachable
-- from Nginx configuration files. Below is definition of the constructor.
--
-- @
--     AggregateServerConf { asPort          :: Int
--                         , asPurgeInterval :: 'TimeInterval'
--                         }
-- @
--
-- The value of /asPort/ corresponds to the TCP port of the internal aggregate
-- server. The /asPurgeInterval/ is the /purge/ interval. An aggregate service
-- should sometimes purge data from worker processes which did not report for a
-- long time. For example, it makes no sense to keep data from workers that
-- have already been terminated. The inactive PIDs get checked every
-- /asPurgeInterval/, and data which correspond to PIDs with timestamps older
-- than /asPurgeInterval/ get removed.
--
-- Be aware that due to limitations of Template Haskell, this name must be
-- imported unqualified!
data AggregateServerConf =
    AggregateServerConf { asPort          :: Int
                        , asPurgeInterval :: TimeInterval
                        } deriving Read

aggregateServer :: (FromJSON a, ToJSON a) =>
    Aggregate a -> ByteString -> AggregateServerConf -> Bool -> IO L.ByteString
aggregateServer a u = ignitionService $ \conf ->
    simpleHttpServe (asConfig $ asPort conf) (asHandler a u conf) >> return ""

asConfig :: Int -> Config Snap a
asConfig p = setPort p
           $ setBind "127.0.0.1"
           $ setAccessLog ConfigNoLog
           $ setErrorLog ConfigNoLog
           $ setVerbose False mempty

asHandler :: (FromJSON a, ToJSON a) =>
    Aggregate a -> ByteString -> AggregateServerConf -> Snap ()
asHandler a u conf =
    route [(B.append "put/" u, Snap.Core.method POST $
              receiveAggregateSnap a $ asPurgeInterval conf
           )
          ,(B.append "get/" u, Snap.Core.method GET $
              sendAggregateSnap a
           )
          ]

receiveAggregateSnap :: FromJSON a => Aggregate a -> TimeInterval -> Snap ()
receiveAggregateSnap a tint =
    handleAggregateExceptions "Exception while receiving aggregate" $ do
        !s <- decode' <$> readRequestBody 65536
        when (isNothing s) $ liftIO $ throwUserError "Unreadable aggregate!"
        liftIO $ receiveAggregate' a (fromJust s) tint
        finishWith emptyResponse

sendAggregateSnap :: ToJSON a => Aggregate a -> Snap ()
sendAggregateSnap a =
    handleAggregateExceptions "Exception while sending aggregate" $ do
        s <- liftIO $ readIORef a
        modifyResponse $ setContentType "application/json"
        writeLBS $ encodeAggregate s

handleAggregateExceptions :: String -> Snap () -> Snap ()
handleAggregateExceptions cmsg = handleAny $ \e ->
    writeErrorResponse 500 $ show (e :: SomeException)
    where writeErrorResponse c msg = do
              modifyResponse $ setResponseStatus c $ T.encodeUtf8 $ T.pack cmsg
              writeBS $ T.encodeUtf8 $ T.pack msg

#endif


-- | Exports a simple aggregate service with specified name and the aggregate
--   type.
--
-- The name of the service can be chosen arbitrarily, however it must be
-- exactly referred from 'reportAggregate' and client requests to the service
-- because the URL of the internal HTTP server contains this.
--
-- The aggregate type must have instances of 'FromJSON' and 'ToJSON' as its
-- objects will be transferred via HTTP in JSON format.
--
-- The service is implemented via 'ngxExportSimpleServiceTyped' with
-- 'AggregateServerConf' as the name of its custom type. This is an
-- 'ignitionService' with an HTTP server based on the [Snap
-- framework](http://snapframework.com/) running inside. The internal HTTP
-- server collects data from worker processes on URL
-- /\/put\/__\<name_of_the_service\>__/ and reports data on URL
-- /\/get\/__\<name_of_the_service\>__/.
ngxExportAggregateService :: String       -- ^ Name of the service
                          -> Name         -- ^ Name of the aggregate type
                          -> Q [Dec]
ngxExportAggregateService f a = do
    let sName = mkName $ "aggregate_storage_" ++ f
        storage = varE sName
#ifdef SNAP_AGGREGATE_SERVER
        nameF = 'aggregateServer
        fName = mkName $ "aggregate_" ++ f
        uName = mkName $ "aggregate_url_" ++ f
#endif
        nameRecv = 'receiveAggregate
        recvName = mkName $ "receiveAggregate_" ++ f
        nameSend = 'sendAggregate
        sendName = mkName $ "sendAggregate_" ++ f
    concat <$> sequence
        [sequence
            [sigD sName [t|Aggregate $(conT a)|]
            ,funD sName
                [clause []
                    (normalB [|unsafePerformIO $ newIORef (0, M.empty)|])
                    []
                ]
            ,pragInlD sName NoInline FunLike AllPhases
#ifdef SNAP_AGGREGATE_SERVER
            ,sigD uName [t|ByteString|]
            ,funD uName [clause [] (normalB [|C8.pack f|]) []]
            ,sigD fName [t|AggregateServerConf -> Bool -> IO L.ByteString|]
            ,funD fName
                [clause []
                    (normalB [|$(varE nameF) $(storage) $(varE uName)|])
                    []
                ]
#endif
            ,sigD recvName [t|L.ByteString -> ByteString -> IO L.ByteString|]
            ,funD recvName
                [clause []
                    (normalB [|$(varE nameRecv) $(storage)|])
                    []
                ]
            ,sigD sendName [t|ByteString -> IO ContentHandlerResult|]
            ,funD sendName
                [clause []
                    (normalB [|$(varE nameSend) $(storage)|])
                    []
                ]
            ]
#ifdef SNAP_AGGREGATE_SERVER
        -- FIXME: name AggregateServerConf must be imported from the user's
        -- module unqualified (see details in NgxExport/Tools.hs, function
        -- ngxExportSimpleService')!
        ,ngxExportSimpleServiceTyped
            fName ''AggregateServerConf SingleShotService
#endif
        ,ngxExportAsyncOnReqBody recvName
        ,ngxExportAsyncHandler sendName
        ]

-- $nginxBasedAggregateService
--
-- Service /simpleService_aggregate_stats/ was implemented using
-- /Snap framework/. Basically, a native Nginx implementation is not easy
-- because the service must listen on a single (not duplicated) file descriptor
-- which is not the case when Nginx spawns more than one worker processes.
-- Running /simpleService_aggregate_stats/ as a shared service is an elegant
-- solution as shared services guarantee that they occupy only one worker at a
-- time. However, /nginx-haskell-module/ provides directive /single_listener/
-- which can be used to apply the required restriction in a custom Nginx virtual
-- server. This directive requires that the virtual server listens with option
-- /reuseport/ and is only available on Linux with socket option
-- /SO_ATTACH_REUSEPORT_CBPF/.
--
-- Exporter 'ngxExportAggregateService' exports additional handlers to build a
-- native Nginx-based aggregate service. Let's replace service
-- /simpleService_aggregate_stats/ from the previous example with such a native
-- Nginx-based aggregate service using /single_listener/ and listening on port
-- /8100/.
--
-- ==== File /nginx.conf/
-- @
-- user                    nobody;
-- worker_processes        2;
--
-- events {
--     worker_connections  1024;
-- }
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_aggregate.so;
--
--     haskell_run_service simpleService_reportStats $hs_reportStats 8100;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         haskell_run updateStats !$hs_updateStats $bytes_sent;
--
--         location \/ {
--             echo Ok;
--         }
--     }
--
--     server {
--         listen       8020;
--         server_name  stat;
--
--         location \/ {
--             allow 127.0.0.1;
--             deny all;
--             proxy_pass http:\/\/127.0.0.1:8100\/get\/stats;
--         }
--     }
--
--     server {
--         listen          8100 __/reuseport/__;
--         server_name     stats;
--
--         __/single_listener on/__;
--
--         location __/\/put\/stats/__ {
--             haskell_run_async_on_request_body __/receiveAggregate_stats/__
--                     $hs_stats \"Min 1\";
--
--             if ($hs_stats = \'\') {
--                 return 400;
--             }
--
--             return 200;
--         }
--
--         location __/\/get\/stats/__ {
--             haskell_async_content __/sendAggregate_stats/__ noarg;
--         }
--     }
-- }
-- @
--
-- Handler /receiveAggregate_stats/ accepts a time interval corresponding to the
-- value of /asPurgeInterval/ from service /simpleService_aggregate_stats/. If
-- the value is not readable (say, /noarg/) then it is defaulted to /Min 5/.
--
-- Notice that the stats server must listen on address /127.0.0.1/ because
-- service /simpleService_reportStats/ reports stats to this address.

-- | Reports data to an aggregate service.
--
-- If reported data is 'Nothing' then data collected on the aggregate service
-- won't alter except that the timestamp associated with the PID of the sending
-- worker process will be updated.
reportAggregate :: ToJSON a => Int          -- ^ Port of the aggregate server
                            -> Maybe a      -- ^ Reported data
                            -> ByteString   -- ^ Name of the aggregate service
                            -> IO ()
reportAggregate p v u =
    handle (const $ return () :: SomeException -> IO ()) $ do
        req <- parseRequest "POST http://127.0.0.1"
        pid <- fromIntegral <$> ngxPid :: IO Int32
        let !req' = req { requestBody = RequestBodyLBS $ encode (pid, v)
                        , port = p
                        , Network.HTTP.Client.path = B.append "put/" u
                        }
        void $ httpNoBody req' httpManager

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

