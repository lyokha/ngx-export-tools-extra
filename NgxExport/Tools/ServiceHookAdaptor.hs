{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.ServiceHookAdaptor
-- Copyright   :  (c) Alexey Radkov 2021
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- A service hook adaptor from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.ServiceHookAdaptor () where

import           NgxExport.Tools

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Monad

hookAdaptor :: ByteString -> Bool -> IO L.ByteString
hookAdaptor = ignitionService $
    const $ forever $ threadDelaySec $ toSec $ Hr 24
ngxExportSimpleService 'hookAdaptor SingleShotService

