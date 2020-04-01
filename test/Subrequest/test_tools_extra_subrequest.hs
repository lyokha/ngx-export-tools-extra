{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraSubrequest where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Subrequest

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

makeRequest :: ByteString -> Bool -> IO L.ByteString
makeRequest = const . makeSubrequest

ngxExportSimpleService 'makeRequest $ PersistentService $ Just $ Sec 10

