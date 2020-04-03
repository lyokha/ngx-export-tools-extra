{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Subrequest
-- Copyright   :  (c) Alexey Radkov 2020
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- Easy HTTP subrequests from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Subrequest (
    -- * Making HTTP subrequests
    -- $makingHTTPSubrequests
                                   makeSubrequest
                                  ,makeSubrequestWithRead
                                  ) where

import           NgxExport
import           NgxExport.Tools

import           Network.HTTP.Client
import           Network.HTTP.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text.Encoding as T
import           Data.CaseInsensitive (mk)
import           Data.Aeson
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
-- makeRequest :: ByteString -> Bool -> IO L.ByteString
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
--             \'{\"uri\": \"http:\/\/httpbin.org\"}\';
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
--                     \'{\"uri\": \"http:\/\/127.0.0.1:8020\/proxy\",
--                       \"headers\": [[\"Custom-Header\", \"$arg_a\"]]}\';
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
--         location \/proxy {
--             internal;
--             proxy_pass http:\/\/backend;
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
-- /backend/ on every request to /location \//. In this subrequest, an HTTP
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
-- Let's do a nasty thing. By injecting a comma into the argument /a/ we shall
-- break JSON parsing.
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

data SubrequestConf =
    SubrequestConf { srMethod          :: ByteString
                   , srUri             :: String
                   , srBody            :: ByteString
                   , srHeaders         :: RequestHeaders
                   , srResponseTimeout :: ResponseTimeout
                   } deriving Read

instance FromJSON SubrequestConf where
    parseJSON = withObject "SubrequestConf" $ \o -> do
        srMethod <- maybeEmpty $ o .:? "method"
        srUri <- o .: "uri"
        srBody <- maybeEmpty $ o .:? "body"
        srHeaders <- map (mk . T.encodeUtf8 *** T.encodeUtf8) <$>
            o .:? "headers" .!= []
        srResponseTimeout <- maybe defaultTimeout (setTimeout . toSec) <$>
            o .:? "timeout"
        return SubrequestConf {..}
        where maybeEmpty = fmap $ maybe "" T.encodeUtf8
              defaultTimeout = responseTimeoutDefault
              setTimeout 0 = responseTimeoutNone
              setTimeout v = responseTimeoutMicro * 1000000

subrequest :: SubrequestConf -> IO L.ByteString
subrequest SubrequestConf {..} = do
    req <- parseUrlThrow srUri
    let req' = if B.null srMethod
                   then req
                   else req { method = srMethod }
        req'' = if B.null srBody
                    then req'
                    else req' { requestBody = RequestBodyBS srBody }
        req''' = if null srHeaders
                     then req''
                     else req'' { requestHeaders = srHeaders }
    responseBody <$>
        httpLbs req''' { responseTimeout = srResponseTimeout } httpManager

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

-- | Makes an HTTP request.
--
-- This is the core function of the /makeSubrequest/ handler. From perspective
-- of an Nginx request, it spawns a subrequest, hence the name. However, this
-- function can also be used to initiate an original HTTP request from a
-- service handler.
--
-- Accepts a JSON object representing an opaque type /SubrequestConf/.
-- The object may contain 4 fields: /method/ (optional, default is /GET/),
-- /uri/ (mandatory), /body/ (optional, default is an empty value), and
-- /headers/ (optional, default is an empty array).
--
-- Examples of subrequest configurations:
--
-- > {"uri": "http://example.com/"}
--
-- > {"uri": "http://127.0.0.1/subreq", "method": "POST", "body": "some value"}
--
-- > {"uri": "http://127.0.0.1/subreq",
-- >     "headers": [["Header1", "Value1"], ["Header2", "Value2"]]}
--
-- Returns the response body if HTTP status of the response is /2xx/, otherwise
-- throws an error. To avoid leakage of error messages into variable handlers,
-- put the corresponding variables into the list of directive
-- /haskell_var_empty_on_error/.
makeSubrequest
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequest = maybe (throwIO SubrequestParseError) subrequest .
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
-- >                , srUri = "http://127.0.0.1/subreq"}
-- >                , srBody = ""
-- >                , srHeaders = [("Header1", "Value1"), ("Header2", "Value2")]
-- >                }
--
-- Notice that unlike JSON parsing, fields of /SubrequestConf/ are not
-- omittable and must be listed in the order shown in the example. Empty
-- /srMethod/ implies /GET/.
makeSubrequestWithRead
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestWithRead = maybe (throwIO SubrequestParseError) subrequest .
    readFromByteString @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestWithRead

