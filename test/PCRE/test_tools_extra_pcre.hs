{-# LANGUAGE TemplateHaskell, LambdaCase #-}

module TestToolsExtraPCRE where

import           NgxExport
import           NgxExport.Tools.PCRE

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

gsubSwapAround :: ByteString -> IO L.ByteString
gsubSwapAround = gsubRegexWith $ const $ \case
    a : d : b : _ -> B.concat [b, d, a]
    _ -> B.empty

ngxExportIOYY 'gsubSwapAround

