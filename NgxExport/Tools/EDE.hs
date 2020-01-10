{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.EDE
-- Copyright   :  (c) Alexey Radkov 2020
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- EDE templates for parsing JSON objects from the more extra tools collection
-- for <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.EDE (
    -- * Rendering JSON objects using EDE templates
    -- $renderingEDETemplates
                            renderEDETemplate
                           ) where

import           NgxExport
import           NgxExport.Tools

import           Text.EDE
import           Text.EDE.Filters
import           Text.PrettyPrint.ANSI.Leijen.Internal (plain)
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Base64.URL
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Aeson (decode, Value)
import           Data.Aeson.Text
import           Network.HTTP.Types.URI (urlEncode)
import           Control.Exception (Exception, throwIO)
import           System.IO.Unsafe
import           System.IO (FilePath)

-- $renderingEDETemplates
--
-- This module allows for sophisticated parsing of JSON objects with [EDE
-- templating language](http://brendanhay.nz/ede/Text-EDE.html#syntax). In terms
-- of module "NgxExport.Tools", it exports a /single-shot/ service
-- __/compileEDETemplates/__ to configure the list of templates parameterized
-- by a simple key, and an asynchronous variable handler __/renderEDETemplate/__
-- for parsing POSTed JSON objects and substitution of extracted data in the
-- provided EDE template.
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_ede.hs/
-- @
-- {-\# OPTIONS_GHC -Wno-unused-imports \#-}
--
-- module TestToolsExtraEDE where
--
-- import NgxExport.Tools.EDE
-- @
--
-- This file does not contain any significant declarations as soon as we do not
-- require anything besides the two exporters. As soon as imported entities are
-- not used, option /-Wno-unused-imports/ was added on the top of the file.
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
--     haskell load \/var\/lib\/nginx\/test_tools_extra_ede.so;
--
--     haskell_run_service __/simpleService_compileEDETemplates/__ $hs_EDETemplates
--             '(\"\/var\/lib\/nginx\/EDE\",
--               [(\"__/user/__\",
--                 \"{{user.id}}\/{{user.ops|__/b64/__}}\/{{resources.path|__/uenc/__}}\")])';
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run_async_on_request_body __/renderEDETemplate/__ $hs_user __/user/__;
--             rewrite ^ \/internal\/user\/$hs_user last;
--         }
--
--         location ~ ^\/internal\/user\/([^\/]+)\/([^\/]+)\/([^\/]+)$ {
--             internal;
--             echo \"User id: $1, options: $2, path: $3\";
--         }
--
--         location ~ \/internal\/user\/(.*) {
--             internal;
--             echo_status 404;
--             echo \"Bad input for user: $1\";
--         }
--     }
-- }
-- @
--
-- There is an EDE template declared by the argument of the service
-- __/simpleService_compileEDETemplates/__. The template will be accessed later
-- in the asynchronous body handler __/renderEDETemplate/__ by the key
-- __/user/__. The path /\/var\/lib\/nginx\/EDE/ can be used in the templates to
-- /include/ more rules from files located inside it, but we do not actually use
-- it here. The rule inside template __/user/__ says: with JSON object, print
-- object /id/ inside a top object /user/, add slash, print object /ops/ inside
-- the top object /user/ filtered by function __/b64/__, add slash, print object
-- /path/ inside a top object /resources/ filtered by function __/uenc/__.
-- Functions /b64/ and /uenc/ are /filters/ in terms of EDE language. There are
-- many filters shipped with EDE, but /b64/ and /uenc/ are defined only in this
-- module. Filter /b64/ encodes the object using /base64url/ encoding, while
-- /uenc/ encodes the object using /URL encoding/.
--
-- So, basically, we used /renderEDETemplate/ to decompose POSTed JSON objects
-- and then /rewrite/ requests to other locations where extracted fields were
-- encoded inside the URL path.
--
-- ==== A simple test
--
-- > $ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"path": "/opt/users"}}' 'http://localhost:8010/'
-- > User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
--
-- Let's try somehow broken input value.
--
-- > $ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"p": "/opt/users"}}' 'http://localhost:8010/'
-- > Bad input for user: EDE ERROR: Text.EDE.parse:1:32 error: variable resources.path doesn't exist.
--
-- Now we got response with HTTP status /404/ and a comprehensive description of
-- what went wrong. To not mess rewrite logic and error responses, variable
-- /$hs_user/ can be listed inside directive /haskell_var_empty_on_error/ in the
-- Nginx configuration.
--
-- @
--     haskell_var_empty_on_error $hs_user;
-- @
--
-- Now errors will only be logged by Nginx in the error log.

type InputTemplates = (FilePath, [(ByteString, ByteString)])
type Templates = HashMap B.ByteString (Result Template)

newtype EDERenderError = EDERenderError String

instance Exception EDERenderError
instance Show EDERenderError where
    show (EDERenderError s) = "EDE ERROR: " ++ s

templates :: IORef Templates
templates = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE templates #-}

compileEDETemplates :: InputTemplates -> Bool -> IO L.ByteString
compileEDETemplates = ignitionService $ \(path, itpls) -> do
    writeIORef templates $
        foldl (\a (k, v) -> HM.insert k (unsafePerformIO $ parseIO path v) a)
            HM.empty itpls
    return ""

ngxExportSimpleServiceTyped 'compileEDETemplates ''InputTemplates
    SingleShotService

filters :: HashMap Id Term
filters = HM.fromList
    ["b64"  @: applyToValue encodeBase64
    ,"uenc" @: applyToValue (T.decodeUtf8 . urlEncode False)
    ]
    where applyToValue :: (ByteString -> Text) -> Value -> Text
          applyToValue f = f . L.toStrict . LT.encodeUtf8 .
              LT.dropAround (== '"') . encodeToLazyText

-- | The core function of the /renderEDETemplate/ exporter.
--
-- Accepts a JSON object written in a 'L.ByteString' and a key to find the EDE
-- template declared by the /compileEDETemplates/ exporter. The function is
-- exported because it can be useful not only in asynchronous body handlers but
-- anywhere else.
renderEDETemplate :: L.ByteString       -- ^ JSON object
                  -> ByteString         -- ^ Key to find the EDE template
                  -> IO L.ByteString
renderEDETemplate v k = do
    tpls <- readIORef templates
    case HM.lookup k tpls of
        Nothing -> throwIO $ EDERenderError $
            "EDE template " ++ C8.unpack k ++ " was not found"
        Just (Failure msg) -> throwIO $ EDERenderError $ showPlain msg
        Just (Success tpl) ->
            case (decode v :: Maybe Value) >>= fromValue of
                Nothing -> throwIO $ EDERenderError $
                    "Failed to decode value '" ++ C8L.unpack v ++ "'"
                Just obj ->
                    case renderWith filters tpl obj of
                        Failure msg -> throwIO $ EDERenderError $ showPlain msg
                        Success r -> return $ LT.encodeUtf8 r
    where showPlain = show . plain

ngxExportAsyncOnReqBody 'renderEDETemplate

