{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications, NumDecimals #-}

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
    -- * Getting full response data from HTTP subrequests
    -- $gettingFullResponse
                                  ,makeSubrequestFull
                                  ,makeSubrequestFullWithRead
                                  ,extractRequestStatusFromFullResponse
                                  ,extractStatusFromFullResponse
                                  ,extractHeaderFromFullResponse
                                  ,extractBodyFromFullResponse
    -- * Forwarding full response data to the client
    -- $forwardingFullResponse
                                  ,notForwardableResponseHeaders
                                  ,contentFromFullResponse
                                  ) where

import           NgxExport
import           NgxExport.Tools

import           Network.HTTP.Client hiding (ResponseTimeout)
import qualified Network.HTTP.Client (HttpExceptionContent (ResponseTimeout))
import           Network.HTTP.Types
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import qualified Data.Text.Encoding as T
import qualified Data.Binary as Binary
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.CaseInsensitive (FoldCase (foldCase), mk, original)
import           Data.Aeson
import           Data.Maybe
import           Data.Word
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
--                     \'{\"uri\": \"http:\/\/127.0.0.1:8010\/proxy\",
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
--             allow 127.0.0.1;
--             deny all;
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

data ResponseTimeout = ResponseTimeoutDefault
                     | ResponseTimeout TimeInterval deriving (Eq, Read)

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
        srResponseTimeout <- maybe ResponseTimeoutDefault ResponseTimeout <$>
            o .:? "timeout"
        return SubrequestConf {..}
        where maybeEmpty = fmap $ maybe "" T.encodeUtf8

subrequest :: (String -> IO Request) ->
    (Response L.ByteString -> L.ByteString) -> SubrequestConf ->
    IO L.ByteString
subrequest parseRequestF buildResponseF SubrequestConf {..} = do
    req <- parseRequestF srUri
    let req' = if B.null srMethod
                   then req
                   else req { method = srMethod }
        req'' = if B.null srBody
                    then req'
                    else req' { requestBody = RequestBodyBS srBody }
        req''' = if null srHeaders
                     then req''
                     else req'' { requestHeaders = srHeaders }
        req'''' = if srResponseTimeout == ResponseTimeoutDefault
                      then req'''
                      else req''' { responseTimeout =
                                        setTimeout srResponseTimeout }
    buildResponseF <$> httpLbs req'''' httpManager
    where setTimeout (ResponseTimeout v)
              | t == 0 = responseTimeoutNone
              | otherwise = responseTimeoutMicro $ t * 1e6
              where t = toSec v
          setTimeout _ = undefined

subrequestBody :: SubrequestConf -> IO L.ByteString
subrequestBody = subrequest parseUrlThrow responseBody

type FullResponse = (Word8, Int, [(ByteString, ByteString)], L.ByteString)

subrequestFull :: SubrequestConf -> IO L.ByteString
subrequestFull = handleAll . subrequest parseRequest buildResponse
    where handleAll = handle $ \e -> return $ Binary.encode @FullResponse $
              case fromException e of
                  Just (HttpExceptionRequest _ c) ->
                      case c of
                          Network.HTTP.Client.ResponseTimeout -> response502
                          ConnectionTimeout -> response502
                          ConnectionFailure _ -> response502
                          _ -> response500
                  _ -> response500
          response500 = (2, 500, [], "")
          response502 = (1, 502, [], "")
          buildResponse r =
              let status = statusCode $ responseStatus r
                  headers = map (first original) $ responseHeaders r
                  body = responseBody r
              in Binary.encode @FullResponse (0, status, headers, body)

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
-- The object may contain 5 fields: /method/ (optional, default is /GET/),
-- /uri/ (mandatory), /body/ (optional, default is an empty value), /headers/
-- (optional, default is an empty array), and /timeout/ (optional, default is
-- the default response timeout of the HTTP manager which is normally 30
-- seconds, use value @{\"tag\": \"Unset\"}@ to disable response timeout
-- completely).
--
-- Examples of subrequest configurations:
--
-- > {"uri": "http://example.com/", "timeout": {"tag": "Sec", "contents": 10}}
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
-- >                }
--
-- Notice that unlike JSON parsing, fields of /SubrequestConf/ are not
-- omittable and must be listed in the order shown in the example. Empty
-- /srMethod/ implies /GET/.
makeSubrequestWithRead
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestWithRead =
    maybe (throwIO SubrequestParseError) subrequestBody .
        readFromByteString @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestWithRead

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
-- same name. Handler __/extractRequestStatusFromFullResponse/__ and the
-- corresponding function can be used to extract the request status: /0/ means
-- that the request was completed without technical errors, /1/ means that the
-- request failed due to connection errors including timeouts, and /2/ means
-- that the request failed due to other errors.
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
--                     \'{\"uri\": \"http:\/\/127.0.0.1:$arg_p\/proxy\",
--                       \"headers\": [[\"Custom-Header\", \"$arg_a\"]]}\';
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
-- ==== File /nginx.conf/: new response header /Subrequest-Header/ in /location \// of server /backend/
-- @
--             add_header Subrequest-Header \"This is response from subrequest\";
-- @
--
-- ==== A simple test
--
-- > $ curl -D- 'http://localhost:8010/full/?a=Value"'
-- > HTTP/1.1 400 Bad Request
-- > Server: nginx/1.17.9
-- > Date: Sat, 04 Apr 2020 12:44:36 GMT
-- > Content-Type: application/octet-stream
-- > Transfer-Encoding: chunked
-- > Connection: keep-alive
-- >
-- > Bad request
--
-- Good. Now we see that adding a comma into a JSON field is a bad request.
--
-- > $ curl -D- 'http://localhost:8010/full/?a=Value'
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
-- > $ curl -D- 'http://localhost:8010/full/?a=Value&p=8020'
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
-- > $ curl -D- 'http://localhost:8010/full/?a=Value&p=8021'
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
-- 'extractRequestStatusFromFullResponse', 'extractStatusFromFullResponse',
-- 'extractHeaderFromFullResponse', and 'extractBodyFromFullResponse'. It also
-- does not throw errors when HTTP status of the response is not /2xx/.
-- Exported on the Nginx level by handler /makeSubrequestFull/.
makeSubrequestFull
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestFull =
    maybe (return $ Binary.encode @FullResponse (2, 400, [], ""))
        subrequestFull .
            readFromByteStringAsJSON @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestFull

-- | Makes an HTTP request.
--
-- The same as 'makeSubrequestWithRead' except it returns a binary encoded
-- response data whose parts must be extracted by handlers made of
-- 'extractRequestStatusFromFullResponse', 'extractStatusFromFullResponse',
-- 'extractHeaderFromFullResponse', and 'extractBodyFromFullResponse'. It also
-- does not throw errors when HTTP status of the response is not /2xx/.
-- Exported on the Nginx level by handler /makeSubrequestFullWithRead/.
makeSubrequestFullWithRead
    :: ByteString       -- ^ Subrequest configuration
    -> IO L.ByteString
makeSubrequestFullWithRead =
    maybe (return $ Binary.encode @FullResponse (2, 400, [], ""))
        subrequestFull .
            readFromByteString @SubrequestConf

ngxExportAsyncIOYY 'makeSubrequestFullWithRead

-- | Extracts the request status from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractRequestStatusFromFullResponse/.
--
-- The request status may be one of the following values:
--
-- - /0/ &#8212; the request was completed without technical errors,
-- - /1/ &#8212; the request failed due to connection errors including timeouts,
-- - /2/ &#8212; the request failed due to other errors.
extractRequestStatusFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractRequestStatusFromFullResponse = C8L.pack . show .
    (\(a, _, _, _) -> a) . (Binary.decode @FullResponse) . L.fromStrict

ngxExportYY 'extractRequestStatusFromFullResponse

-- | Extracts the HTTP status from an encoded response.
--
-- Must be used to extract response data encoded by 'makeSubrequestFull' or
-- 'makeSubrequestFullWithRead'. Exported on the Nginx level by handler
-- /extractStatusFromFullResponse/.
extractStatusFromFullResponse
    :: ByteString       -- ^ Encoded HTTP response
    -> L.ByteString
extractStatusFromFullResponse = C8L.pack . show .
    (\(_, a, _, _) -> a) . (Binary.decode @FullResponse) . L.fromStrict

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
        hs = (\(_, _, a, _) -> map (first mk) a) $
            Binary.decode @FullResponse $ L.fromStrict b
    in maybe "" L.fromStrict $ lookup h hs

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
    (\(_, _, _, a) -> a) . (Binary.decode @FullResponse) . L.fromStrict

ngxExportYY 'extractBodyFromFullResponse

-- $forwardingFullResponse
--
-- Data encoded in the full response can be translated to 'ContentHandlerResult'
-- and forwarded downstream to the client in directive /haskell_content/.
-- Handler __/fromFullResponse/__ performs such a translation. Not all response
-- headers are allowed being forwarded downstream, and thus the handler deletes
-- response headers with names listed in set 'notForwardableResponseHeaders' as
-- well as all headers with names starting with /X-Accel-/ before sending the
-- response to the client. The set of not forwardable response headers can be
-- customized in function 'contentFromFullResponse'.
--
-- Let's forward responses in location /\/full/ when argument /proxy/ in the
-- client request's URI is equal to /yes/.
--
-- ==== File /nginx.conf/: forward responses from location /\/full/
-- @
--             if ($arg_proxy = yes) {
--                 haskell_content fromFullResponse $hs_subrequest;
--                 break;
--             }
-- @
--
-- ==== A simple test
--
-- > $ curl -D- 'http://localhost:8010/full/?a=Value&p=8020&proxy=yes'
-- > HTTP/1.1 200 OK
-- > Server: nginx/1.17.9
-- > Date: Fri, 24 Jul 2020 13:14:33 GMT
-- > Content-Type: application/octet-stream
-- > Content-Length: 37
-- > Connection: keep-alive
-- > Subrequest-Header: This is response from subrequest
-- >
-- > In backend, Custom-Header is 'Value'

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

-- | Translates encoded response to 'ContentHandlerResult'.
--
-- The translated data can be forwarded to the client by a simple handler based
-- on this function in directive /haskell_content/. Handler /fromFullResponse/
-- forwards the response to the client after deleting headers listed in set
-- 'notForwardableResponseHeaders' and headers with names starting with
-- /X-Accel-/.
contentFromFullResponse
    :: HashSet HeaderName   -- ^ Set of not forwardable response headers
    -> Bool                 -- ^ Do not forward /X-Accel-.../ response headers
    -> ByteString           -- ^ Encoded HTTP response
    -> ContentHandlerResult
contentFromFullResponse headersToDelete deleteXAccel v =
    let (_, st, hs, b) = Binary.decode @FullResponse $ L.fromStrict v
        hs' = map (first mk) hs
        ct = fromMaybe "" $ lookup (mk "Content-Type") hs'
        hs'' = filter (\(n, _) -> not $
                          mk n `HS.member` headersToDelete ||
                              deleteXAccel &&
                                  foldCase "X-Accel-" `B.isPrefixOf` foldCase n
                      ) hs
    in (b, ct, st, hs'')

fromFullResponse :: ByteString -> ContentHandlerResult
fromFullResponse = contentFromFullResponse notForwardableResponseHeaders True

ngxExportHandler 'fromFullResponse

