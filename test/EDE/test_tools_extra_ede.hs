{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraEDE where

import           NgxExport
import           NgxExport.Tools.EDE ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy (LazyByteString)
import qualified Network.HTTP.Types.URI as URI

urlDecode :: ByteString -> LazyByteString
urlDecode = L.fromStrict . URI.urlDecode False

ngxExportYY 'urlDecode

