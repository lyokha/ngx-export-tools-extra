{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraSubrequest where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Subrequest

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

subrequestFromService :: ByteString -> Bool -> IO L.ByteString
subrequestFromService = const . subrequest

ngxExportSimpleService 'subrequestFromService $
    PersistentService $ Just $ Sec 10

