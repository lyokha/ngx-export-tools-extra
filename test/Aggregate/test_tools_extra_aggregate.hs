{-# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module TestToolsExtraAggregate where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Aggregate

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.Maybe
import           Data.IORef
import           System.IO.Unsafe
import           GHC.Generics

data Stats = Stats { bytesSent :: Int
                   , requests :: Int
                   , meanBytesSent :: Int
                   } deriving Generic
instance FromJSON Stats
instance ToJSON Stats

stats :: IORef Stats
stats = unsafePerformIO $ newIORef $ Stats 0 0 0
{-# NOINLINE stats #-}

updateStats :: ByteString -> IO C8L.ByteString
updateStats s = do
    let cbs = readFromByteString @Int s
    modifyIORef' stats $ \(Stats bs rs _) ->
        let !nbs = bs + fromMaybe 0 cbs
            !nrs = rs + 1
            !nmbs = nbs `div` nrs
        in Stats nbs nrs nmbs
    return ""
ngxExportIOYY 'updateStats

reportStats :: Int -> Bool -> IO C8L.ByteString
reportStats = deferredService $ \port -> do
    s <- readIORef stats
    reportAggregate port (Just s) "stats"
    return ""
ngxExportSimpleServiceTyped 'reportStats ''Int $
    PersistentService $ Just $ Sec 5

ngxExportAggregateService "stats" ''Stats

