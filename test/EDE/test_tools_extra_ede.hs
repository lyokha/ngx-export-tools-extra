{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraEDE where

import           NgxExport
import           NgxExport.Tools.EDE

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types.URI as URI
import           Control.Arrow

renderEDETemplateFromFreeValue :: ByteString -> IO L.ByteString
renderEDETemplateFromFreeValue = uncurry (flip renderEDETemplate) .
    second (L.fromStrict . C8.tail) . C8.break (== '|')

ngxExportIOYY 'renderEDETemplateFromFreeValue

urlDecode :: ByteString -> L.ByteString
urlDecode = L.fromStrict . URI.urlDecode False

ngxExportYY 'urlDecode

