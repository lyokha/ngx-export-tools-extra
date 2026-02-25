{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module TestToolsExtraPCRE where

import           NgxExport
import           NgxExport.Tools.PCRE

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import           Data.ByteString.Lazy (LazyByteString)

gsubSwapAround :: ByteString -> IO LazyByteString
gsubSwapAround = gsubRegexWith $ const $ \case
    a : d : b : _ -> B.concat [b, d, a]
    _ -> B.empty

ngxExportIOYY 'gsubSwapAround

