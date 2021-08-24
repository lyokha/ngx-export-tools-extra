{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraPCRE where

import           NgxExport
import           NgxExport.Tools.PCRE

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

gsubSwapAround :: ByteString -> IO L.ByteString
gsubSwapAround = gsubRegexWith $ \_ (a : d : b : _) -> B.concat [b, d, a]

ngxExportIOYY 'gsubSwapAround

