{-# LANGUAGE CPP, TemplateHaskell, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.EDE
-- Copyright   :  (c) Alexey Radkov 2020-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
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
                           ,renderEDETemplateWith
                           ,renderEDETemplateFromFreeValue
                           ) where

import           NgxExport
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.SplitService

import           Text.EDE
import           Text.EDE.Filters
#ifdef EDE_USE_PRETTYPRINTER
#if MIN_VERSION_prettyprinter(1,7,0)
import           Prettyprinter (unAnnotate)
#else
import           Data.Text.Prettyprint.Doc (unAnnotate)
#endif
#else
import           Text.PrettyPrint.ANSI.Leijen (plain)
#endif
import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.ByteString.Base64.URL
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as LT
import           Data.Aeson (encode, decode, Value (String))
import           Network.HTTP.Types.URI (urlEncode)
import           Control.Exception (Exception, throwIO)
import           Control.Arrow
import           System.IO.Unsafe

-- $renderingEDETemplates
--
-- This module allows for complex parsing of JSON objects with [EDE templating
-- language](http://hackage.haskell.org/package/ede/docs/Text-EDE.html). In
-- terms of module "NgxExport.Tools.SimpleService", it exports a /single-shot/
-- service __/compileEDETemplates/__ to configure a list of templates
-- parameterized by a simple key, and two variable handlers
-- __/renderEDETemplate/__ and __/renderEDETemplateFromFreeValue/__ for parsing
-- JSON objects and substitution of extracted data into provided EDE templates.
-- The former handler is /asynchronous/ and suitable for parsing JSON objects
-- POSTed in a request body, while the latter is /synchronous/ and can parse
-- JSON objects contained in Nginx variables.
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_ede.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell \#-}
--
-- module TestToolsExtraEDE where
--
-- import           NgxExport
-- import           NgxExport.Tools.EDE ()
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString.Lazy as L
-- import qualified Network.HTTP.Types.URI as URI
--
-- urlDecode :: ByteString -> L.ByteString
-- urlDecode = L.fromStrict . URI.urlDecode False
--
-- 'ngxExportYY' \'urlDecode
-- @
--
-- We are going to use /urlDecode/ to decode JSON  values contained in HTTP
-- cookies. Notice that we are not using any Haskell declarations from module
-- /NgxExport.Tools.EDE/ while still need to import this to access the three
-- handlers from the Nginx configuration. This situation is quite valid though
-- not usual to /ghc/, and to make it keep silence, an explicit empty import
-- list was added at the end of the import stanza.
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
--         location ~ ^\/internal\/user\/(EDE\\ ERROR:.*) {
--             internal;
--             echo_status 404;
--             echo \"Bad input: $1\";
--         }
--
--         location ~ ^\/internal\/user\/([^\/]+)\/([^\/]+)\/([^\/]+)$ {
--             internal;
--             echo \"User id: $1, options: $2, path: $3\";
--         }
--
--         location ~ ^\/internal\/user\/(.*) {
--             internal;
--             echo_status 404;
--             echo \"Unexpected input: $1\";
--         }
--
--         location \/cookie {
--             haskell_run urlDecode $hs_cookie_user $cookie_user;
--             haskell_run __/renderEDETemplateFromFreeValue/__ $hs_user_from_cookie
--                     __/user/__|$hs_cookie_user;
--             rewrite ^ \/internal\/user\/$hs_user_from_cookie last;
--         }
--     }
-- }
-- @
--
-- There is an EDE template declared by the argument of service
-- __/simpleService_compileEDETemplates/__. The template will be accessed later
-- in the asynchronous body handler __/renderEDETemplate/__ with key __/user/__.
-- Path /\/var\/lib\/nginx\/EDE/ can be used in the templates to /include/ more
-- rules from files located inside it, but we do not actually use this here.
--
-- The rule inside template /user/ says: with given JSON object,
--
-- * print object /id/ inside a top object /user/,
-- * print /slash/,
-- * print object /ops/ inside the top object /user/ filtered by function /b64/,
-- * print /slash/,
-- * print object /path/ inside a top object /resources/ filtered by function
-- /uenc/.
--
-- Functions /b64/ and /uenc/ are /polymorphic filters/ in terms of EDE
-- language. There are many filters shipped with EDE, but /b64/ and /uenc/ were
-- defined in this module.
--
-- * __/b64/__ encodes a JSON object using /base64url/ encoding
-- * __/uenc/__ encodes a JSON object using /URL encoding/ rules
--
-- So, basically, we used /renderEDETemplate/ to decompose POSTed JSON objects
-- and then /rewrite/ requests to other locations where the URL path after
-- substitution of the extracted and then encoded into variable /$hs_user/
-- fields points to. Handler /renderEDETemplateFromFreeValue/ in location
-- /\/cookie/ does the same but reads JSON objects from HTTP cookie /user/.
--
-- ==== A simple test
--
-- > $ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"path": "/opt/users"}}' 'http://localhost:8010/'
-- > User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
--
-- Let's try to send a broken (in any meaning) input value.
--
-- > $ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"p": "/opt/users"}}' 'http://localhost:8010/'
-- > Bad input: EDE ERROR: Text.EDE.parse:1:32 error: variable resources.path doesn't exist.
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
-- Now the variable will always be empty on errors, while the errors will still
-- be logged by Nginx in the error log.
--
-- Let's read user data encoded in HTTP cookie /user/.
--
-- > $ curl -b 'user=%7B%22user%22%3A%20%7B%22id%22%20%3A%20%22user1%22%2C%20%22ops%22%3A%20%5B%22op1%22%2C%20%22op2%22%5D%7D%2C%20%22resources%22%3A%20%7B%22path%22%3A%20%22%2Fopt%2Fusers%22%7D%7D' 'http://localhost:8010/cookie'
-- > User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers

type InputTemplates = (FilePath, [(ByteString, ByteString)])
type Templates = HashMap ByteString (Result Template)

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
          applyToValue f (String t) = f $ T.encodeUtf8 t
          applyToValue f v = f $ L.toStrict $ encode v

-- | Renders an EDE template from a JSON object.
--
-- This is the core function of the /renderEDETemplate/ handler. Accepts a JSON
-- object written in a 'L.ByteString' and a key to find a compiled EDE template
-- declared by the /compileEDETemplates/ handler.
renderEDETemplate :: L.ByteString       -- ^ JSON object
                  -> ByteString         -- ^ Key to find the EDE template
                  -> IO L.ByteString
renderEDETemplate = renderEDETemplateWith decode

-- | Renders an EDE template with a custom decoding function.
--
-- This function can be used for templating from any configuration language
-- which is translatable to Aeson's 'Value'.
renderEDETemplateWith
    :: (L.ByteString -> Maybe Value)    -- ^ Decoding function
    -> L.ByteString                     -- ^ JSON object
    -> ByteString                       -- ^ Key to find the EDE template
    -> IO L.ByteString
renderEDETemplateWith fdec v k = do
    tpls <- readIORef templates
    case HM.lookup k tpls of
        Nothing -> throwIO $ EDERenderError $
            "EDE template " ++ C8.unpack k ++ " was not found"
        Just (Failure msg) -> throwIO $ EDERenderError $ showPlain msg
        Just (Success tpl) ->
            case fdec v >>= fromValue of
                Nothing -> throwIO $ EDERenderError $
                    "Failed to decode value '" ++ C8L.unpack v ++ "'"
                Just obj ->
                    case renderWith filters tpl obj of
                        Failure msg -> throwIO $ EDERenderError $ showPlain msg
                        Success r -> return $ LT.encodeUtf8 r
    where showPlain = show .
#ifdef EDE_USE_PRETTYPRINTER
              unAnnotate
#else
              plain
#endif

ngxExportAsyncOnReqBody 'renderEDETemplate

-- | Renders an EDE template from a JSON object.
--
-- This is the core function of the /renderEDETemplateFromFreeValue/ handler.
-- Accepts a JSON object attached after the search key and a vertical bar such
-- as /key|$hs_json/.
renderEDETemplateFromFreeValue
    :: ByteString           -- ^ Key to find the EDE template, and JSON object
    -> IO L.ByteString
renderEDETemplateFromFreeValue = uncurry (flip renderEDETemplate) .
    second (L.fromStrict . C8.tail) . C8.break (== '|')

ngxExportIOYY 'renderEDETemplateFromFreeValue

