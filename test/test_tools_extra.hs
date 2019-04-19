{-# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module TestToolsExtra where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Aggregate

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.Maybe
import           Data.IORef
import           Control.Monad
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

showAsLazyByteString :: Show a => a -> L.ByteString
showAsLazyByteString = C8L.pack . show

updateStats :: ByteString -> IO C8L.ByteString
updateStats s = do
    let mv = readFromByteStringAsJSON @Stats s
    when (isJust mv) $ do
        let v@Stats {..} = fromJust mv
        modifyIORef' stats $ \(Stats bs rs mbs) ->
            Stats (bs + bytesSent) (rs + requests) (mbs + meanBytesSent)
    return ""
ngxExportIOYY 'updateStats

reportStats :: ByteString -> Bool -> IO C8L.ByteString
reportStats = deferredService $ const $ do
    s <- readIORef stats
    reportAggregate 8020 (Just s) "stats"
    return ""
ngxExportSimpleService 'reportStats $ PersistentService $ Just $ Sec 5

ngxExportAggregateService "stats" ''Stats

