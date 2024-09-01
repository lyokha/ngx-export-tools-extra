{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications, TupleSections, LambdaCase, NumDecimals #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Subrequest
-- Copyright   :  (c) Alexey Radkov 2020-2024
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-- Easy HTTP subrequests from the more extra tools collection for
-- <https://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Subrequest (
    -- * Making HTTP subrequests
    -- $makingHTTPSubrequests
                                   makeSubrequest
                                  ,makeSubrequestWithRead
    -- * Internal HTTP subrequests via Unix domain sockets
    -- $internalHTTPSubrequests

    -- * HTTP subrequests with a custom HTTP manager
    -- $subrequestsWithCustomManager
                                  ,registerCustomManager
    -- * Getting full response data from HTTP subrequests
    -- $gettingFullResponse
                                  ,makeSubrequestFull
                                  ,makeSubrequestFullWithRead
                                  ,extractStatusFromFullResponse
                                  ,extractHeaderFromFullResponse
                                  ,extractBodyFromFullResponse
                                  ,extractExceptionFromFullResponse
    -- * Forwarding full response data to the client
    -- $forwardingFullResponse
                                  ,notForwardableResponseHeaders
                                  ,contentFromFullResponse
    -- * Making bridged HTTP subrequests
    -- $makingBridgedHTTPSubrequests
                                  ,makeBridgedSubrequest
                                  ,makeBridgedSubrequestWithRead
                                  ,makeBridgedSubrequestFull
                                  ,makeBridgedSubrequestFullWithRead
                                  ) where

import           NgxExport
import           NgxExport.Tools.Read
import           NgxExport.Tools.Combinators
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.TimeInterval

import           Network.HTTP.Client hiding (ResponseTimeout)
import qualified Network.HTTP.Client (HttpExceptionContent (ResponseTimeout))
import           Network.HTTP.Client.TLS (newTlsManager)
import           Network.HTTP.Client.BrReadWithTimeout
import           Network.HTTP.Types
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.IORef
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Binary as Binary
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.CaseInsensitive hiding (map)
import           Data.Function
import           Data.Aeson
import           Data.Maybe
import           Data.List
import           Control.Arrow
import           Control.Exception
import           System.IO.Unsafe

-- $makingHTTPSubrequests
--
-- Using asynchronous variable handlers and services together with the HTTP
-- client from "Network.HTTP.Client" allows making HTTP subrequests easily.
-- This module provides such functionality by exporting asynchronous variable
-- handlers __/makeSubrequest/__ and __/makeSubrequestWithRead/__, and functions
-- 'makeSubrequest' and 'makeSubrequestWithRead' to build custom handlers.
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_subrequest.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
--
-- module TestToolsExtraSubrequest where
--
-- import           NgxExport
-- import           NgxExport.Tools
-- import           NgxExport.Tools.Subrequest
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
--
-- makeRequest :: ByteString -> 'NgxExportService'
-- __/makeRequest/__ = const . 'makeSubrequest'
--
-- 'ngxExportSimpleService' \'makeRequest $ 'PersistentService' $ Just $ 'Sec' 10
-- @
--
-- Handler /makeRequest/ will be used in a /periodical/ service which will
-- retrieve data from a specified URI every 10 seconds.
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
--     haskell load \/var\/lib\/nginx\/test_tools_extra_subrequest.so;
--
--     upstream backend {
--         server 127.0.0.1:8020;
--     }
--
--     haskell_run_service __/simpleService_makeRequest/__ $hs_service_httpbin
--             \'{\"uri\": \"https:\/\/httpbin.org\"}\';
--
--     haskell_var_empty_on_error $hs_subrequest;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run_async __/makeSubrequest/__ $hs_subrequest
--                     \'{\"uri\": \"http:\/\/127.0.0.1:8010\/proxy\"
--                      ,\"headers\": [[\"Custom-Header\", \"$arg_a\"]]
--                      }\';
--
--             if ($hs_subrequest = \'\') {
--                 echo_status 404;
--                 echo \"Failed to perform subrequest\";
--                 break;
--             }
--
--             echo -n $hs_subrequest;
--         }
--
--         location ~ ^\/proxy(.*) {
--             allow 127.0.0.1;
--             deny all;
--             proxy_pass http:\/\/backend$1;
--         }
--
--         location \/httpbin {
--             echo $hs_service_httpbin;
--         }
--     }
--
--     server {
--         listen       8020;
--         server_name  backend;
--
--         location\ / {
--             set $custom_header $http_custom_header;
--             echo \"In backend, Custom-Header is \'$custom_header\'\";
--         }
--     }
-- }
-- @
--
-- Configurations of subrequests are defined via JSON objects which contain URI
-- and other relevant data such as HTTP method, request body and headers. In
-- this configuration we are running a periodical service which gets contents of
-- /httpbin.org/ every 10 seconds, and doing a subrequest to a virtual server
-- /backend/ on every request to location /\//. In this subrequest, an HTTP
-- header /Custom-Header/ is sent to the backend with value equal to the value
-- of argument /a/ from the client request's URI.
--
-- It is worth noting that making HTTP subrequests to the own Nginx service
-- (e.g. via /127.0.0.1/) allows for leveraging well-known advantages of Nginx
-- such as load-balancing via upstreams as it is happening in this example.
--
-- ==== A simple test
--
-- > $ curl -s 'http://localhost:8010/httpbin' | head
-- > <!DOCTYPE html>
-- > <html lang="en">
-- >
-- > <head>
-- >     <meta charset="UTF-8">
-- >     <title>httpbin.org</title>
-- >     <link href="https://fonts.googleapis.com/css?family=Open+Sans:400,700|Source+Code+Pro:300,600|Titillium+Web:400,600,700"
-- >         rel="stylesheet">
-- >     <link rel="stylesheet" type="text/css" href="/flasgger_static/swagger-ui.css">
-- >     <link rel="icon" type="image/png" href="/static/favicon.ico" sizes="64x64 32x32 16x16" />
--
-- > $ curl 'http://localhost:8010/?a=Value'
-- > In backend, Custom-Header is 'Value'
--
-- Let's do a nasty thing. By injecting a double quote into the argument /a/ we
-- shall break JSON parsing.
--
-- > $ curl -D- 'http://localhost:8010/?a=Value"'
-- > HTTP/1.1 404 Not Found
-- > Server: nginx/1.17.9
-- > Date: Mon, 30 Mar 2020 14:42:42 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Failed to perform subrequest

data SubrequestParseError = SubrequestParseError deriving Show

instance Exception SubrequestParseError

data BridgeParseError = BridgeParseError deriving Show

instance Exception BridgeParseError

data UDSNotConfiguredError = UDSNotConfiguredError deriving Show

instance Exception UDSNotConfiguredError

newtype ManagerNotConfiguredError =
    ManagerNotConfiguredError ByteString deriving Show

instance Exception ManagerNotConfiguredError

data ResponseTimeout = ResponseTimeoutDefault
                     | ResponseTimeout TimeInterval deriving (Eq, Read)

data ConnectionManager = Default
                       | UDS
                       | Custom ByteString deriving (Eq, Read)

data SubrequestConf =
    SubrequestConf { srMethod          :: ByteString
                   , srUri             :: String
                   , srBody            :: L.ByteString
                   , srHeaders         :: RequestHeaders
                   , srResponseTimeout :: ResponseTimeout
                   , srManager         :: ConnectionManager
                   } deriving Read

instance FromJSON SubrequestConf where
    parseJSON = withObject "SubrequestConf" $ \o -> do
        srMethod <- maybe "" T.encodeUtf8 <$> o .:? "method"
        srUri <- o .: "uri"
        srBody <- maybe "" TL.encodeUtf8 <$> o .:? "body"
        srHeaders <- map (mk . T.encodeUtf8 *** T.encodeUtf8) <$>
            o .:? "headers" .!= []
        srResponseTimeout <- maybe ResponseTimeoutDefault ResponseTimeout <$>
            o .:? "timeout"
        srManager <- maybe Default (\case
                                        "default" -> Default
                                        "uds" -> UDS
                                        v -> Custom $ T.encodeUtf8 v
                                   ) <$> o .:? "manager"
        return SubrequestConf {..}

data BridgeConf = BridgeConf { bridgeSource :: SubrequestConf
                             , bridgeSink :: SubrequestConf
                             } deriving Read

instance FromJSON BridgeConf where
    parseJSON = withObject "BridgeConf" $ \o -> do
        bridgeSource <- o .: "source"
        bridgeSink <- o .: "sink"
        return BridgeConf {..}

makeRequest :: SubrequestConf -> Request -> Request
makeRequest SubrequestConf {..} req =
    req { method = if B.null srMethod
                       then method req
                       else srMethod
        , requestBody = if L.null srBody
                            then requestBody req
                            else RequestBodyLBS srBody
        , requestHeaders = unionBy ((==) `on` fst) srHeaders $
                               requestHeaders req
        , responseTimeout = if srResponseTimeout == ResponseTimeoutDefault
                                then responseTimeout req
                                else setTimeout srResponseTimeout
        }
    where setTimeout (ResponseTimeout v)
              | t == 0 = responseTimeoutNone
              | otherwise = responseTimeoutMicro $ t * 1e6
              where t = toSec v
          setTimeout _ = undefined

subrequest :: (String -> IO Request) ->
    (Response L.ByteString -> L.ByteString) -> SubrequestConf ->
    IO L.ByteString
subrequest parseRequestF buildResponseF sub@SubrequestConf {..} = do
    man <- getManager sub
    req <- parseRequestF srUri
    buildResponseF <$> httpLbsBrReadWithTimeout (makeRequest sub req) man

subrequestBody :: SubrequestConf -> IO L.ByteString
subrequestBody = subrequest parseUrlThrow responseBody

type FullResponse = (Int, [(ByteString, ByteString)], L.ByteString, ByteString)

handleFullResponse :: IO L.ByteString -> IO L.ByteString
handleFullResponse = handle $ \e -> do
    let msg = C8.pack $ show e
        responseXXX = (, [], "", msg)
        response500 = responseXXX 500
        response502 = responseXXX 502
    return $ Binary.encode @FullResponse $
        case fromException e of
            Just (HttpExceptionRequest _ c) ->
                case c of
                    Network.HTTP.Client.ResponseTimeout -> response502
                    ConnectionTimeout -> response502
                    ConnectionFailure _ -> response502
                    StatusCodeException r _ ->
                        let status = statusCode $ responseStatus r
                        in responseXXX status
                    _ -> response500
            _ -> response500

buildFullResponse :: Response L.ByteString -> L.ByteString
buildFullResponse r =
    let status = statusCode $ responseStatus r
        headers = map (first original) $ responseHeaders r
        body = responseBody r
    in Binary.encode @FullResponse (status, headers, body, "")

subrequestFull :: SubrequestConf -> IO L.ByteString
subrequestFull = handleFullResponse . subrequest parseRequest buildFullResponse

httpManager :: Manager
httpManager = unsafePerformIO newTlsManager
{-# NOINLINE httpManager #-}

httpUDSManager :: IORef (Maybe Manager)
httpUDSManager = unsafePerformIO $ newIORef Nothing
{-# NOINLINE httpUDSManager #-}

httpCustomManager :: IORef (HashMap ByteString Manager)
httpCustomManager = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE httpCustomManager #-}

getManager :: SubrequestConf -> IO Manager
getManager SubrequestConf {..} =
    case srManager of
        Default ->
            return httpManager
        UDS ->
            fromMaybe (throw UDSNotConfiguredError) <$>
                readIORef httpUDSManager
        Custom k ->
            fromMaybe (throw $ ManagerNotConfiguredError k) . HM.lookup k <$>
                readIORef httpCustomManager

-- | Makes an HTTP request.
--
-- This is the core function of the /makeSubrequest/ handler. From perspective
-- of an Nginx request, it spawns a subrequest, hence the name. However, this
-- function can also be used to initiate an original HTTP request from a
-- service handler.
--
-- Accepts a JSON object representing an opaque type /SubrequestConf/. The
-- object may contain the following fields: /method/ (optional, default is
-- /GET/), /uri/ (mandatory), /body/ (optional, default is an empty string),
-- /headers/ (optional, default is an empty list), /timeout/ (optional, default
-- is the default response timeout of the HTTP manager which is normally 30
-- seconds, use value @{\"tag\": \"Unset\"}@ to disable response timeout
-- completely), and /manager/ (an optional value which links to an HTTP manager
-- that will serve connections, default is /default/ which links to the internal
-- TLS-aware manager).
--
-- Examples of subrequest configurations:
--
-- > {"uri": "http://example.com/", "timeout": {"tag": "Sec", "contents": 10}}
--
-- > {"uri": "http://127.0.0.1/subreq", "method": "POST", "body": "some value"}
--
-- > {"uri": "http://127.0.0.1/subreq"
-- > ,"headers": [["Header1", "Value1"], ["Header2", "Value2"]]
-- > }
--
-- Note that the response timeout is in effect until receiving the response
-- headers as well as in between the successive body read events.
--
-- Returns the response body if HTTP status of the response is /2xx/, otherwise
-- throws an error. To avoid leakage of error messages into variable handlers,
-- put the corresponding variables into the list of directive
-- /haskell_var_empty_on_error/.
makeSubrequest
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequest =
    maybe (throwIO SubrequestParseError) subrequestBody .
        readFromByteStringAsJSON @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequest

-- | Makes an HTTP request.
--
-- Behaves exactly as 'makeSubrequest' except it parses Haskell terms
-- representing /SubrequestConf/ with 'read'. Exported on the Nginx level by
-- handler /makeSubrequestWithRead/.
--
-- An example of a subrequest configuration:
--
-- > SubrequestConf { srMethod = ""
-- >                , srUri = "http://127.0.0.1/subreq"
-- >                , srBody = ""
-- >                , srHeaders = [("Header1", "Value1"), ("Header2", "Value2")]
-- >                , srResponseTimeout = ResponseTimeout (Sec 10)
-- >                , srManager = Default
-- >                }
--
-- Notice that unlike JSON parsing, fields of /SubrequestConf/ are not
-- omittable and must be listed in the order shown in the example. Empty
-- /srMethod/ implies /GET/. Values of /srManager/ can be /Default/, /UDS/, or
-- /Custom \"key\"/ where /key/ is an arbitrary key bound to a custom HTTP
-- manager.
makeSubrequestWithRead
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestWithRead =
    maybe (throwIO SubrequestParseError) subrequestBody .
        readFromByteString @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestWithRead

-- $internalHTTPSubrequests
--
-- Making HTTP subrequests to the own Nginx service via the loopback interface
-- (e.g. via /127.0.0.1/) has disadvantages of being neither very fast (if
-- compared with various types of local data communication channels) nor very
-- secure. Unix domain sockets is a better alternative in this sense. This
-- module has support for them by providing configuration service
-- __/simpleService_configureUDS/__ where path to the socket can be set, and
-- setting field /manager/ to value /uds/ in the subrequest configuration.
--
-- To extend the previous example for using with Unix domain sockets, the
-- following declarations should be added.
--
-- ==== File /nginx.conf/: configuring the Unix domain socket
-- @
--     haskell_run_service __/simpleService_configureUDS/__ $hs_service_uds
--             \'__/UDSConf/__ {__/udsPath/__ = \"\/tmp\/backend.sock\"}\';
-- @
--
-- /UDSConf/ is an opaque type containing only one field /udsPath/ with the path
-- to the socket.
--
-- ==== File /nginx.conf/: new location /\/uds/ in server /main/
-- @
--         location \/uds {
--             haskell_run_async __/makeSubrequest/__ $hs_subrequest
--                     \'{\"uri\": \"http:\/\/backend_proxy\/\"
--                      ,\"headers\": [[\"Custom-Header\", \"$arg_a\"]]
--                      ,\"__/manager/__\": \"__uds__\"
--                      }\';
--
--             if ($hs_subrequest = \'\') {
--                 echo_status 404;
--                 echo \"Failed to perform subrequest\";
--                 break;
--             }
--
--             echo -n $hs_subrequest;
--         }
-- @
--
-- ==== File /nginx.conf/: new virtual server /backend_proxy/
-- @
--     server {
--         listen       unix:\/tmp\/backend.sock;
--         server_name  backend_proxy;
--
--         location \/ {
--             proxy_pass http:\/\/backend;
--         }
--     }
-- @
--
-- The server listens on the Unix domain socket with the path configured in
-- service /simpleService_configureUDS/.
--
-- ==== A simple test
--
-- > $ curl 'http://localhost:8010/uds?a=Value'
-- > In backend, Custom-Header is 'Value'

newtype UDSConf = UDSConf { udsPath :: FilePath } deriving Read

configureUDS :: UDSConf -> NgxExportService
configureUDS = ignitionService $ \UDSConf {..} -> voidHandler $ do
    man <- newManager defaultManagerSettings
               { managerRawConnection = return $ openUDS udsPath }
    writeIORef httpUDSManager $ Just man
    where openUDS path _ _ _ = do
              s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
              S.connect s (S.SockAddrUnix path)
              makeConnection (SB.recv s 4096) (SB.sendAll s) (S.close s)

ngxExportSimpleServiceTyped 'configureUDS ''UDSConf SingleShotService

-- $subrequestsWithCustomManager
--
-- To serve subrequests, a custom HTTP manager can be implemented and then
-- registered in a custom service handler with 'registerCustomManager'. To
-- enable this manager in a subrequest configuration, use field /manager/
-- with the key that was bound to the manager in 'registerCustomManager'.
--
-- For example, let's implement a custom UDS manager which will serve
-- connections via Unix Domain Sockets as in the previous section.
--
-- ==== File /test_tools_extra_subrequest_custom_manager.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, OverloadedStrings \#-}
--
-- module TestToolsExtraSubrequestCustomManager where
--
-- import           NgxExport.Tools
-- import           NgxExport.Tools.Subrequest
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
--
-- import           Network.HTTP.Client
-- import qualified Network.Socket as S
-- import qualified Network.Socket.ByteString as SB
-- import qualified Data.ByteString.Char8 as C8
--
-- configureUdsManager :: ByteString -> 'NgxExportService'
-- __/configureUdsManager/__ = 'ignitionService' $ \\path -> 'voidHandler' $ do
--     man <- newManager defaultManagerSettings
--                { managerRawConnection = return $ openUDS path }
--     'registerCustomManager' \"__myuds__\" man
--     where openUDS path _ _ _ = do
--               s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
--               S.connect s (S.SockAddrUnix $ C8.unpack path)
--               makeConnection (SB.recv s 4096) (SB.sendAll s) (S.close s)
--
-- 'ngxExportSimpleService' \'configureUdsManager 'SingleShotService'
-- @
--
-- ==== File /nginx.conf/: configuring the custom manager
-- @
--     haskell_run_service __/simpleService_configureUdsManager/__ $hs_service_myuds
--             \'\/tmp\/myuds.sock\';
-- @
--
-- ==== File /nginx.conf/: new location /\/myuds/ in server /main/
-- @
--         location \/myuds {
--             haskell_run_async __/makeSubrequest/__ $hs_subrequest
--                     \'{\"uri\": \"http:\/\/backend_proxy_myuds\/\"
--                      ,\"headers\": [[\"Custom-Header\", \"$arg_a\"]]
--                      ,\"__/manager/__\": \"__myuds__\"
--                      }\';
--
--             if ($hs_subrequest = \'\') {
--                 echo_status 404;
--                 echo \"Failed to perform subrequest\";
--                 break;
--             }
--
--             echo -n $hs_subrequest;
--         }
-- @
--
-- ==== File /nginx.conf/: new virtual server /backend_proxy_myuds/
-- @
--     server {
--         listen       unix:\/tmp\/myuds.sock;
--         server_name  backend_proxy_myuds;
--
--         location \/ {
--             proxy_pass http:\/\/backend;
--         }
--     }
-- @

-- | Registers a custom HTTP manager with a given key.
--
-- The right place to register a custom manager is a custom service handler or
-- the initialization hook (see 'ngxExportInitHook') which runs soon after the
-- start of an Nginx worker process. Registered managers can then be referred to
-- from subrequest configurations by the key in field /manager/ (in JSON-encoded
-- configurations) or as /srManager = Custom \"key\"/ (in /read/-encoded
-- configurations).
--
-- Below is an example of a JSON-encoded subrequest configuration.
--
-- > {"uri": "http://example.com/", "manager": "mymanager"}
--
-- Note that keys /default/ and /uds/ have special meaning in field /manager/:
-- they denote internal HTTP and UDS managers respectively.
registerCustomManager
    :: ByteString       -- ^ Key
    -> Manager          -- ^ Manager
    -> IO ()
registerCustomManager = (modifyIORef' httpCustomManager .) . HM.insert

-- $gettingFullResponse
--
-- Handlers /makeSubrequest/ and /makeSubrequestWithRead/ return response body
-- of subrequests skipping the response status and headers. To retrieve full
-- data from a response, use another pair of asynchronous variable handlers and
-- functions: __/makeSubrequestFull/__ and __/makeSubrequestFullWithRead/__,
-- and 'makeSubrequestFull' and 'makeSubrequestFullWithRead' respectively.
--
-- Unlike the simple body handlers, there is no sense of using the corresponding
-- variables directly as they are binary encoded values. Instead, the response
-- status, headers and the body must be extracted using handlers
-- __/extractStatusFromFullResponse/__, __/extractHeaderFromFullResponse/__,
-- and __/extractBodyFromFullResponse/__ which are based on functions of the
-- same name. Handler __/extractExceptionFromFullResponse/__ and the
-- corresponding function can be used to extract the error message if an
-- exception has happened while making the subrequest: the value is empty if
-- there was no exception.
--
-- Let's extend our example with these handlers.
--
-- File /test_tools_extra_subrequest.hs/ does not have any changes as we are
-- going to use exported handlers only.
--
-- ==== File /nginx.conf/: new location /\/full/ in server /main/
-- @
--         location \/full {
--             haskell_run_async __/makeSubrequestFull/__ $hs_subrequest
--                     \'{\"uri\": \"http:\/\/127.0.0.1:$arg_p\/proxy\"
--                      ,\"headers\": [[\"Custom-Header\", \"$arg_a\"]]
--                      }\';
--
--             haskell_run __/extractStatusFromFullResponse/__ $hs_subrequest_status
--                     $hs_subrequest;
--
--             haskell_run __/extractHeaderFromFullResponse/__ $hs_subrequest_header
--                     subrequest-header|$hs_subrequest;
--
--             haskell_run __/extractBodyFromFullResponse/__ $hs_subrequest_body
--                     $hs_subrequest;
--
--             if ($hs_subrequest_status = 400) {
--                 echo_status 400;
--                 echo \"Bad request\";
--                 break;
--             }
--
--             if ($hs_subrequest_status = 500) {
--                 echo_status 500;
--                 echo \"Internal server error while making subrequest\";
--                 break;
--             }
--
--             if ($hs_subrequest_status = 502) {
--                 echo_status 502;
--                 echo \"Backend unavailable\";
--                 break;
--             }
--
--             if ($hs_subrequest_status != 200) {
--                 echo_status 404;
--                 echo \"Subrequest status: $hs_subrequest_status\";
--                 break;
--             }
--
--             echo    \"Subrequest status: $hs_subrequest_status\";
--             echo    \"Subrequest-Header: $hs_subrequest_header\";
--             echo -n \"Subrequest body: $hs_subrequest_body\";
--         }
-- @
--
-- Now we can recognize HTTP response statuses of subrequests and handle them
-- differently. We also can read a response header /Subrequest-Header/.
--
-- ==== File /nginx.conf/: new response header /Subrequest-Header/ in location /\// of server /backend/
-- @
--             add_header Subrequest-Header \"This is response from subrequest\";
-- @
--
-- ==== A simple test
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value"'
-- > HTTP/1.1 400 Bad Request
-- > Server: nginx/1.17.9
-- > Date: Sat, 04 Apr 2020 12:44:36 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Bad request
--
-- Good. Now we see that injecting a double quote into a JSON field makes a bad
-- request.
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value'
-- > HTTP/1.1 500 Internal Server Error
-- > Server: nginx/1.17.9
-- > Date: Sat, 04 Apr 2020 12:47:11 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Internal server error while making subrequest
--
-- This is also good. Now we are going to define port of the backend server via
-- argument /$arg_p/. Skipping this makes URI look unparsable
-- (/http:\/\/127.0.0.1:\//) which leads to the error.
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value&p=8020'
-- > HTTP/1.1 200 OK
-- > Server: nginx/1.17.9
-- > Date: Sat, 04 Apr 2020 12:52:03 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Subrequest status: 200
-- > Subrequest-Header: This is response from subrequest
-- > Subrequest body: In backend, Custom-Header is 'Value'
--
-- Finally, we are getting a good response with all the response data decoded
-- correctly.
--
-- Let's try another port.
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value&p=8021'
-- > HTTP/1.1 502 Bad Gateway
-- > Server: nginx/1.17.9
-- > Date: Sat, 04 Apr 2020 12:56:02 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Backend unavailable
--
-- Good. There is no server listening on port 8021.

-- | Makes an HTTP request.
--
-- The same as 'makeSubrequest' except it returns a binary encoded response data
-- whose parts must be extracted by handlers made of
-- 'extractStatusFromFullResponse', 'extractHeaderFromFullResponse',
-- 'extractBodyFromFullResponse', and 'extractExceptionFromFullResponse'. It
-- does not throw any exceptions outside. Exported on the Nginx level by handler
-- /makeSubrequestFull/.
makeSubrequestFull
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestFull =
    maybe (return $
              Binary.encode @FullResponse
                  (400, [], "", "Unreadable subrequest data")
          ) subrequestFull . readFromByteStringAsJSON @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestFull

-- | Makes an HTTP request.
--
-- The same as 'makeSubrequestWithRead' except it returns a binary encoded
-- response data whose parts must be extracted by handlers made of
-- 'extractStatusFromFullResponse', 'extractHeaderFromFullResponse',
-- 'extractBodyFromFullResponse', and 'extractExceptionFromFullResponse'. It
-- does not throw any exceptions outside. Exported on the Nginx level by handler
-- /makeSubrequestFullWithRead/.
makeSubrequestFullWithRead
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestFullWithRead =
    maybe (return $
              Binary.encode @FullResponse
                  (400, [], "", "Unreadable subrequest data")
          ) subrequestFull . readFromByteString @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestFullWithRead

-- | Extracts the HTTP status from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractStatusFromFullResponse/.
extractStatusFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractStatusFromFullResponse = C8L.pack . show .
    (\(a, _, _, _) -> a) . Binary.decode @FullResponse . L.fromStrict

ngxExportYY 'extractStatusFromFullResponse

-- | Extracts a specified header from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractHeaderFromFullResponse/.
--
-- Expects that the encoded response data is attached after the name of the
-- header and a vertical bar such as /Header-Name|$hs_body/. The lookup of the
-- header name is case-insensitive. Returns an empty value if the header was not
-- found.
extractHeaderFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractHeaderFromFullResponse v =
    let (h, b) = mk *** C8.tail $ C8.break ('|' ==) v
        (_, hs, _, _) = Binary.decode @FullResponse $ L.fromStrict b
    in maybe "" L.fromStrict $ lookup h $ map (first mk) hs

ngxExportYY 'extractHeaderFromFullResponse

-- | Extracts the body from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractBodyFromFullResponse/.
extractBodyFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractBodyFromFullResponse =
    (\(_, _, a, _) -> a) . Binary.decode @FullResponse . L.fromStrict

ngxExportYY 'extractBodyFromFullResponse

-- | Extracts the exception from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractExceptionFromFullResponse/.
--
-- The empty value implies that there was no exception while making the
-- subrequest. Non-/2xx/ responses are not regarded as exceptions as well.
extractExceptionFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractExceptionFromFullResponse = L.fromStrict .
    (\(_, _, _, a) -> a) . Binary.decode @FullResponse . L.fromStrict

ngxExportYY 'extractExceptionFromFullResponse

-- $forwardingFullResponse
--
-- Data encoded in the full response can be translated to 'ContentHandlerResult'
-- and forwarded downstream to the client in directive /haskell_content/.
-- Handlers __/fromFullResponse/__ and __/fromFullResponseWithException/__
-- perform such a translation. Not all response headers are allowed being
-- forwarded downstream, and thus the handlers delete response headers with
-- names listed in set 'notForwardableResponseHeaders' as well as all headers
-- with names starting with /X-Accel-/ before sending the response to the
-- client. The set of not forwardable response headers can be customized in
-- function 'contentFromFullResponse'.
--
-- Let's forward responses in location /\/full/ when argument /proxy/ in the
-- client request's URI is equal to /yes/.
--
-- ==== File /nginx.conf/: forward responses from location /\/full/
-- @
--             set $proxy_with_exception $arg_proxy$arg_exc;
--
--             if ($proxy_with_exception = yesyes) {
--                 haskell_content __/fromFullResponseWithException/__ $hs_subrequest;
--                 break;
--             }
--
--             if ($arg_proxy = yes) {
--                 haskell_content __/fromFullResponse/__ $hs_subrequest;
--                 break;
--             }
-- @
--
-- ==== A simple test
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value&p=8020&proxy=yes'
-- > HTTP/1.1 200 OK
-- > Server: nginx/1.17.9
-- > Date: Fri, 24 Jul 2020 13:14:33 GMT
-- > Content-Type: application/octet-stream
-- > Content-Length: 37
-- > Connection: keep-alive
-- > Subrequest-Header: This is response from subrequest
-- >
-- > In backend, Custom-Header is 'Value'
--
-- Now let's get an error message in the response after feeding a wrong port
-- value.
--
-- > $ curl -D- 'http://localhost:8010/full?a=Value&p=8021&proxy=yes&exc=yes'
-- > HTTP/1.1 502 Bad Gateway
-- > Server: nginx/1.19.4
-- > Date: Mon, 14 Dec 2020 08:24:22 GMT
-- > Content-Length: 593
-- > Connection: keep-alive
-- >
-- > HttpExceptionRequest Request {
-- >   host                 = "127.0.0.1"
-- >   port                 = 8021
-- >   secure               = False
-- >   requestHeaders       = [("Custom-Header","Value")]
-- >   path                 = "/proxy"
-- >   queryString          = ""
-- >   method               = "GET"
-- >   proxy                = Nothing
-- >   rawBody              = False
-- >   redirectCount        = 10
-- >   responseTimeout      = ResponseTimeoutDefault
-- >   requestVersion       = HTTP/1.1
-- >   proxySecureMode      = ProxySecureWithConnect
-- > }
-- >  (ConnectionFailure Network.Socket.connect: <socket: 31>: does not exist (Connection refused))

-- | Default set of not forwardable response headers.
--
-- HTTP response headers that won't be forwarded to the client in handler
-- /fromFullResponse/. The set contains /Connection/, /Content-Length/, /Date/,
-- /Keep-Alive/, /Last-Modified/, /Server/, /Transfer-Encoding/, and
-- /Content-Type/ headers (the latter gets reset in the handler's result value).
-- If this set is not satisfactory, then handler /fromFullResponse/ must be
-- replaced with a custom handler based on 'contentFromFullResponse' with a
-- customized set of not forwardable response headers.
notForwardableResponseHeaders :: HashSet HeaderName
notForwardableResponseHeaders = HS.fromList $
    map mk ["Connection"
           ,"Content-Length"
           ,"Content-Type"
           ,"Date"
           ,"Keep-Alive"
           ,"Last-Modified"
           ,"Server"
           ,"Transfer-Encoding"
           ,"X-Pad"
           ]

deleteHeaders :: HashSet HeaderName -> Bool -> ResponseHeaders ->
    ResponseHeaders
deleteHeaders headersToDelete deleteXAccel =
    filter (\(n, _) -> not $
               n `HS.member` headersToDelete ||
                   deleteXAccel &&
                       foldCase "X-Accel-" `B.isPrefixOf` foldedCase n
           )

-- | Translates encoded response to 'ContentHandlerResult'.
--
-- The translated data can be forwarded to the client by a simple handler based
-- on this function in directive /haskell_content/. Handlers /fromFullResponse/
-- and /fromFullResponseWithException/ forward the response to the client after
-- deleting headers listed in set 'notForwardableResponseHeaders' and headers
-- with names starting with /X-Accel-/. The two handlers differ in the response
-- composing function: the former always returns the response body of the
-- subrequest while the latter returns the error message in the response body if
-- an exception has happened during the subrequest.
contentFromFullResponse
    :: HashSet HeaderName   -- ^ Set of not forwardable response headers
    -> Bool                 -- ^ Do not forward /X-Accel-.../ response headers
    -> (L.ByteString -> ByteString -> L.ByteString)
                            -- ^ Function to compose response body and exception
    -> ByteString           -- ^ Encoded HTTP response
    -> ContentHandlerResult
contentFromFullResponse headersToDelete deleteXAccel f v =
    let (st, hs, b, e) = Binary.decode @FullResponse $ L.fromStrict v
        hs' = map (first mk) hs
        ct = fromMaybe "" $ lookup (mk "Content-Type") hs'
        hs'' = deleteHeaders headersToDelete deleteXAccel hs'
    in (f b e, ct, st, map (first original) hs'')

fromFullResponse :: ByteString -> ContentHandlerResult
fromFullResponse =
    contentFromFullResponse notForwardableResponseHeaders True const

ngxExportHandler 'fromFullResponse

fromFullResponseWithException :: ByteString -> ContentHandlerResult
fromFullResponseWithException =
    contentFromFullResponse notForwardableResponseHeaders True f
    where f "" = L.fromStrict
          f b = const b

ngxExportHandler 'fromFullResponseWithException

-- $makingBridgedHTTPSubrequests
--
-- A bridged HTTP subrequest streams the response body from the /source/ end of
-- the /bridge/ to the /sink/ end. Both source and sink are subrequests
-- configured with the familiar type /SubrequestConf/. They comprise another
-- opaque type /BridgeConf/. The bridge abstraction is useful when some data is
-- going to be copied from some source to some destination.
--
-- A bridge can be configured using handlers __/makeBridgedSubrequest/__,
-- __/makeBridgedSubrequestWithRead/__, __/makeBridgedSubrequestFull/__, and
-- __/makeBridgedSubrequestFullWithRead/__ derived from the functions with the
-- same names.
--
-- Let's extend our example with bridged subrequests.
--
-- ==== File /test_tools_extra_subrequest.hs/: auxiliary read body handler
-- @
-- reqBody :: L.ByteString -> ByteString -> IO L.ByteString
-- reqBody = const . return
--
-- 'ngxExportAsyncOnReqBody' \'reqBody
-- @
--
-- In this example, we are going to collect the request body at the sink end
-- with an auxiliary handler /reqBody/.
--
-- ==== File /nginx.conf/: upstream /sink/
-- @
--     upstream sink {
--         server 127.0.0.1:8030;
--     }
-- @
--
-- ==== File /nginx.conf/: new location /\/bridge/ in server /main/
-- @
--         location \/bridge {
--             haskell_run_async __/makeBridgedSubrequestFull/__ $hs_subrequest
--                     \'{\"__/source/__\":
--                         {\"uri\": \"http:\/\/127.0.0.1:$arg_p\/proxy\/bridge\"
--                         ,\"headers\": [[\"Custom-Header\", \"$arg_a\"]]
--                         }
--                      ,\"__/sink/__\":
--                         {\"uri\": \"http:\/\/sink_proxy\/echo\"
--                         ,\"manager\": \"uds\"
--                         }
--                      }\';
--
--             if ($arg_exc = yes) {
--                 haskell_content __/fromFullResponseWithException/__ $hs_subrequest;
--                 break;
--             }
--
--             haskell_content __/fromFullResponse/__ $hs_subrequest;
--         }
-- @
--
-- ==== File /nginx.conf/: new location /\/bridge/ in server /backend/
-- @
--         location \/bridge {
--             set $custom_header $http_custom_header;
--             add_header Subrequest-Header \"This is response from subrequest\";
--             echo \"The response may come in chunks!\";
--             echo \"In backend, Custom-Header is \'$custom_header\'\";
--         }
-- @
--
-- ==== File /nginx.conf/: new servers /sink_proxy/ and /sink/
-- @
--     server {
--         listen       unix:\/tmp\/backend.sock;
--         server_name  sink_proxy;
--
--         location \/ {
--             proxy_pass http:\/\/sink;
--         }
--     }
--
--     server {
--         listen       8030;
--         server_name  sink;
--
--         location \/echo {
--             haskell_run_async_on_request_body reqBody $hs_rb noarg;
--             add_header Bridge-Header
--                     \"This response was bridged from subrequest\";
--             echo \"Here is the bridged response:\";
--             echo -n $hs_rb;
--         }
--     }
-- @
--
-- Upon receiving a request with URI /\/bridge/ at the main server, we are going
-- to connect to the /source/ with the same URI at the server with port equal to
-- argument /$arg_p/, and then stream its response body to a /sink/ with URI
-- /\/echo/ via proxy server /sink_proxy/. Using an internal Nginx proxy server
-- for the sink end of the bridge is necessary if the sink end does not
-- recognize chunked HTTP requests! Note also that /method/ of the sink
-- subrequest is always /POST/ independently of whether or not and how exactly
-- it was specified.
--
-- The source end puts into the bridge channel its response headers except those
-- listed in 'notForwardableResponseHeaders' and those with names starting with
-- /X-Accel-/. The request headers listed in the sink configuration get also
-- sent: their values override the values of the headers of the same names sent
-- in the response from the source end of the bridge.
--
-- Bridged HTTP subrequests have transactional semantics: any errors occurred at
-- either end of a bridge make the whole subrequest fail. Responses from the
-- source end of a bridge with /non-2xx/ status codes are regarded as a failure.
--
-- In this example, after receiving all streamed data the sink collects the
-- request body in variable /$hs_rb/ and merely sends it back as a response to
-- the original bridged subrequest. Then this response gets decoded with
-- handlers /fromFullResponse/ or /fromFullResponseWithException/ and finally
-- returned in the response to the client.
--
-- ==== A simple test
--
-- > $ curl -D- 'http://localhost:8010/bridge?a=Value&p=8010&exc=yes'
-- > HTTP/1.1 200 OK
-- > Server: nginx/1.19.4
-- > Date: Tue, 19 Oct 2021 13:12:46 GMT
-- > Content-Type: application/octet-stream
-- > Content-Length: 100
-- > Connection: keep-alive
-- > Bridge-Header: This response was bridged from subrequest
-- >
-- > Here is the bridged response:
-- > The response may come in chunks!
-- > In backend, Custom-Header is 'Value'
--
-- A negative case.
--
-- > $ curl -D- 'http://localhost:8010/bridge?a=Value&p=8021&exc=yes'
-- > HTTP/1.1 502 Bad Gateway
-- > Server: nginx/1.19.4
-- > Date: Tue, 19 Oct 2021 13:16:18 GMT
-- > Content-Length: 600
-- > Connection: keep-alive
-- >
-- > HttpExceptionRequest Request {
-- >   host                 = "127.0.0.1"
-- >   port                 = 8021
-- >   secure               = False
-- >   requestHeaders       = [("Custom-Header","Value")]
-- >   path                 = "/proxy/bridge"
-- >   queryString          = ""
-- >   method               = "GET"
-- >   proxy                = Nothing
-- >   rawBody              = False
-- >   redirectCount        = 10
-- >   responseTimeout      = ResponseTimeoutDefault
-- >   requestVersion       = HTTP/1.1
-- >   proxySecureMode      = ProxySecureWithConnect
-- > }
-- >  (ConnectionFailure Network.Socket.connect: <socket: 32>: does not exist (Connection refused))

makeStreamingRequest :: GivesPopper () -> SubrequestConf -> Request -> Request
makeStreamingRequest givesPopper conf req =
    makeRequest conf { srMethod = "POST" , srBody = "" }
                req { requestBody = RequestBodyStreamChunked givesPopper }

bridgedSubrequest :: (String -> IO Request) ->
    (Response L.ByteString -> L.ByteString) -> BridgeConf ->
    IO L.ByteString
bridgedSubrequest parseRequestF buildResponseF BridgeConf {..} = do
    manIn <- getManager bridgeSource
    manOut <- getManager bridgeSink
    -- BEWARE: a non-2xx response from the bridge source will throw
    -- StatusCodeException with this status which finally will be returned as
    -- the status code of the whole bridged subrequest
    reqIn <- parseUrlThrow $ srUri bridgeSource
    reqOut <- parseRequestF $ srUri bridgeSink
    withResponse (makeRequest bridgeSource reqIn) manIn $ \respIn -> do
        let reqOut' = reqOut { requestHeaders =
                                   deleteHeaders
                                       notForwardableResponseHeaders
                                       True (responseHeaders respIn)
                             }
            tmo = fromResponseTimeout reqIn manIn
            givesPopper needsPopper = needsPopper $
                brReadWithTimeout tmo reqIn $ responseBody respIn
        buildResponseF <$>
            httpLbsBrReadWithTimeout
                (makeStreamingRequest givesPopper bridgeSink reqOut') manOut

bridgedSubrequestBody :: BridgeConf -> IO L.ByteString
bridgedSubrequestBody = bridgedSubrequest parseUrlThrow responseBody

bridgedSubrequestFull :: BridgeConf -> IO L.ByteString
bridgedSubrequestFull =
    handleFullResponse . bridgedSubrequest parseRequest buildFullResponse

-- | Makes a bridged HTTP request.
--
-- This is the core function of the /makeBridgedSubrequest/ handler. From
-- perspective of an Nginx request, it spawns two subrequests connecting the two
-- ends of a /bridge/: the /source/ and the /sink/, hence the name. The
-- connection between the bridge ends is implemented via 'GivesPopper' and
-- 'RequestBodyStreamChunked' which means that the server bound at the sink end
-- must be capable of processing chunked requests.
--
-- Accepts a JSON object representing an opaque type /BridgeConf/ with mandatory
-- fields /source/ and /sink/.
--
-- An example of a bridge configuration:
--
-- > {"source":
-- >      {"uri": "http://example.com/"
-- >      ,"headers": [["Header1", "Value1"], ["Header2", "Value2"]]
-- >      }
-- > ,"sink":
-- >      {"uri": "http://sink_proxy/"
-- >      ,"manager": "uds"
-- >      }
-- > }
--
-- The sink method is always /POST/ while its body is always empty independently
-- of whether or not and how exactly they were specified. The sink response
-- timeout should be big enough to fulfill streaming of the response from the
-- source to the sink.
--
-- Returns the response body of the sink if HTTP status of the response is
-- /2xx/, otherwise throws an error. To avoid leakage of error messages into
-- variable handlers, put the corresponding variables into the list of directive
-- /haskell_var_empty_on_error/.
makeBridgedSubrequest
    :: ByteString       -- ^ Bridge configuration
    -> IO L.ByteString
makeBridgedSubrequest =
    maybe (throwIO BridgeParseError) bridgedSubrequestBody .
        readFromByteStringAsJSON @BridgeConf

ngxExportAsyncIOYY 'makeBridgedSubrequest

-- | Makes a bridged HTTP request.
--
-- Behaves exactly as 'makeBridgedSubrequest' except it parses Haskell terms
-- representing /BridgeConf/ with 'read'. Exported on the Nginx level by
-- handler /makeBridgedSubrequestWithRead/.
--
-- An example of a bridge configuration:
--
-- > BridgeConf
-- > { bridgeSource = SubrequestConf
-- >       { srMethod = ""
-- >       , srUri = "http://127.0.0.1/source"
-- >       , srBody = ""
-- >       , srHeaders = [("Header1", "Value1"), ("Header2", "Value2")]
-- >       , srResponseTimeout = ResponseTimeout (Sec 10)
-- >       , srManager = Default
-- >       }
-- > , bridgeSink = SubrequestConf
-- >       { srMethod = ""
-- >       , srUri = "http://127.0.0.1/sink"
-- >       , srBody = ""
-- >       , srHeaders = []
-- >       , srResponseTimeout = ResponseTimeout (Sec 30)
-- >       , srManager = Default
-- >       }
-- > }
--
-- The sink method is always /POST/ while its body is always empty independently
-- of how exactly they were specified. The sink response timeout should be big
-- enough to fulfill streaming of the response from the source to the sink.
--
-- Notice that unlike JSON parsing, fields of /SubrequestConf/ comprising
-- /bridgeSource/ and /bridgeSink/ are not omittable and must be listed in the
-- order shown in the example. As well, fields /bridgeSource/ and /bridgeSink/
-- must be listed in this order.
makeBridgedSubrequestWithRead
    :: ByteString       -- ^ Bridge configuration
    -> IO L.ByteString
makeBridgedSubrequestWithRead =
    maybe (throwIO BridgeParseError) bridgedSubrequestBody .
        readFromByteString @BridgeConf

ngxExportAsyncIOYY 'makeBridgedSubrequestWithRead

-- | Makes a bridged HTTP request.
--
-- The same as 'makeBridgedSubrequest' except it returns a binary encoded
-- response data whose parts must be extracted by handlers made of
-- 'extractStatusFromFullResponse', 'extractHeaderFromFullResponse',
-- 'extractBodyFromFullResponse', and 'extractExceptionFromFullResponse'. It
-- does not throw any exceptions outside. Exported on the Nginx level by handler
-- /makeBridgedSubrequestFull/.
makeBridgedSubrequestFull
    :: ByteString       -- ^ Bridge configuration
    -> IO L.ByteString
makeBridgedSubrequestFull =
    maybe (return $
              Binary.encode @FullResponse
                  (400, [], "", "Unreadable bridged subrequest data")
          ) bridgedSubrequestFull . readFromByteStringAsJSON @BridgeConf

ngxExportAsyncIOYY 'makeBridgedSubrequestFull

-- | Makes a bridged HTTP request.
--
-- The same as 'makeBridgedSubrequestWithRead' except it returns a binary
-- encoded response data whose parts must be extracted by handlers made of
-- 'extractStatusFromFullResponse', 'extractHeaderFromFullResponse',
-- 'extractBodyFromFullResponse', and 'extractExceptionFromFullResponse'. It
-- does not throw any exceptions outside. Exported on the Nginx level by handler
-- /makeBridgedSubrequestFullWithRead/.
makeBridgedSubrequestFullWithRead
    :: ByteString       -- ^ Bridge configuration
    -> IO L.ByteString
makeBridgedSubrequestFullWithRead =
    maybe (return $
              Binary.encode @FullResponse
                  (400, [], "", "Unreadable bridged subrequest data")
          ) bridgedSubrequestFull . readFromByteString @BridgeConf

ngxExportAsyncIOYY 'makeBridgedSubrequestFullWithRead

