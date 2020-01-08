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
-- EDE templates from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.EDE (
    -- * Rendering EDE templates
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

type InputTemplates = (FilePath, [(ByteString, ByteString)])
type Templates = HashMap B.ByteString (Result Template)

newtype EDERenderError =
    EDERenderError String  -- ^ Contains the message to log

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

renderEDETemplate :: L.ByteString -> ByteString -> IO L.ByteString
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

