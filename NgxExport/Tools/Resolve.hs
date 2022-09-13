{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, LambdaCase #-}
{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Resolve
-- Copyright   :  (c) Alexey Radkov 2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- DNS resolve utilities from the more extra tools collection
-- for <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport.Tools.Resolve (
                                collectA
                               ,collectSRV
                               ,collectUpstreams
                               ,signalUpconf
                               ) where

import           NgxExport
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.TimeInterval

import           Network.DNS
import           Network.HTTP.Client
import qualified Data.ByteString.Lazy as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Maybe
import           Data.Aeson
import           Data.Function
import           Data.List
import           Data.Bits
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Arrow
import           Control.Monad
import           System.IO.Unsafe

type UKey = Text            -- key to upstream description

type Url = Text             -- normally must start with /
type Destination = Text     -- IP address or domain name

type TTLRange = (TTL, TTL)

data Upstream = QueryA UKey [Name]
              | QuerySRV PriorityList
              deriving Read

data PriorityList = SinglePriority UKey Name
                  | PriorityList [UKey] Name
                  deriving Read

data UData = UData { upstream     :: Upstream
                   , uMaxFails    :: Int
                   , uFailTimeout :: Int
                   } deriving Read

data Conf = Conf { upstreams       :: [UData]
                 , maxWait         :: TimeInterval
                 , waitOnException :: TimeInterval
                 } deriving Read

newtype Upconf = Upconf { upconfAddr :: (Url, Destination) } deriving Read

data ServerData = ServerData { sAddr        :: Destination
                             , sWeight      :: Maybe Int
                             , sMaxFails    :: Maybe Int
                             , sFailTimeout :: Maybe Int
                             } deriving (Show, Eq, Ord)

instance FromJSON ServerData where
    parseJSON = withObject "server_options" $ \o -> do
        sAddr        <- o .:  "addr"
        sWeight      <- o .:? "weight"
        sMaxFails    <- o .:? "max_fails"
        sFailTimeout <- o .:? "fail_timeout"
        return ServerData {..}

instance ToJSON ServerData where
    toJSON ServerData {..} =
        object $ catMaybes [ pure $ "addr"   .=      sAddr
                           , ("weight"       .=) <$> sWeight
                           , ("max_fails"    .=) <$> sMaxFails
                           , ("fail_timeout" .=) <$> sFailTimeout
                           ]

type CollectedData = (TTL, Map UKey [ServerData])

collectedData :: IORef CollectedData
collectedData = unsafePerformIO $ newIORef (TTL 0, M.empty)
{-# NOINLINE collectedData #-}

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

getResponse :: Text -> (Request -> IO (Response L.ByteString)) ->
    IO L.ByteString
getResponse url = fmap responseBody . (parseRequest (T.unpack url) >>=)

getUrl :: Text -> IO L.ByteString
getUrl url = getResponse url $ flip httpLbs httpManager

queryHTTP :: Text -> Text -> IO L.ByteString
queryHTTP = (getUrl .) . flip mkAddr
    where mkAddr = (("http://" `T.append`) .) . T.append

collectA :: TTLRange -> Name -> IO (TTL, [IPv4])
collectA rTTL@(lTTL, _) name = handleCollectError lTTL $ do
    !srv <- queryA name
    return (minimumTTL rTTL $ map fst srv, map snd srv)

collectSRV :: TTLRange -> Name -> IO (TTL, [SRV IPv4])
collectSRV rTTL@(lTTL, _) name = handleCollectError lTTL $ do
    !srv <- querySRV name
    !srv' <- mapConcurrently
                 ((\s@SRV {..} -> do
                     (t, is) <- collectA rTTL srvTarget
                     return (t, map (\v -> s { srvTarget = v }) is)
                  ) . snd
                 ) srv
    return (min (minimumTTL rTTL $ map fst srv)
                (minimumTTL rTTL $ map fst srv')
           ,concatMap snd srv'
           )

handleCollectError :: TTL -> IO (TTL, [a]) -> IO (TTL, [a])
handleCollectError lTTL =
    handle (\(e :: SomeException) -> do
               writeIORef collectedData (lTTL, M.empty)
               throwIO e
           )

minimumTTL :: TTLRange -> [TTL] -> TTL
minimumTTL (lTTL, _) [] = lTTL
minimumTTL (_, hTTL) srv = minimum $ hTTL : srv

showIPv4 :: IPv4 -> String
showIPv4 (IPv4 w) =
  shows ((w !>>. 24) .&. 0xff) . ('.' :) .
  shows ((w !>>. 16) .&. 0xff) . ('.' :) .
  shows ((w !>>.  8) .&. 0xff) . ('.' :) $
  shows ( w          .&. 0xff)
  ""

ipv4ToServerData :: UData -> IPv4 -> ServerData
ipv4ToServerData UData {..} i =
    ServerData (T.pack $ show i) Nothing (Just uMaxFails) (Just uFailTimeout)

srvToServerData :: UData -> SRV IPv4 -> ServerData
srvToServerData UData {..} SRV {..} =
    ServerData (T.pack $ showAddr srvTarget srvPort)
        (Just $ fromIntegral srvWeight) (Just uMaxFails) (Just uFailTimeout)
    where showAddr i p = showIPv4 i ++ ':' : show p

-- BEWARE: ServerData must be ordered because libresolv may rotate elements!
collectData :: TTLRange -> UData -> IO CollectedData
collectData rTTL ud@(UData (QueryA k us) _ _) = do
    a <- mapConcurrently (collectA rTTL) us
    return $
        minimum *** M.singleton k . concat $
            foldr (\(t, s) (ts, ss) ->
                      (t : ts, sort (map (ipv4ToServerData ud) s) : ss)
                  ) ([], []) a
collectData rTTL ud@(UData (QuerySRV (SinglePriority k u)) _ _) = do
    (ttl, srv) <- collectSRV rTTL u
    return (ttl, M.singleton k $ sort $ map (srvToServerData ud) srv)
collectData (lTTL, _) (UData (QuerySRV (PriorityList [] _)) _ _) =
    return (lTTL, M.empty)
collectData rTTL ud@(UData (QuerySRV (PriorityList pl u)) _ _ ) = do
    (ttl, srv) <- collectSRV rTTL u
    let srv' = zip (withTrail pl) $ partitionByPriority srv
    return (ttl
           ,M.fromList $ map (second $ sort . map (srvToServerData ud)) srv'
           )
    where partitionByPriority =
              groupBy ((==) `on` srvPriority) . sortOn srvPriority
          withTrail = uncurry (++) . (id &&& repeat . last)

collectUpstreams :: Conf -> Bool -> IO L.ByteString
collectUpstreams Conf {..} = const $ do
    (TTL wTTL, !old) <- readIORef collectedData
    when (wTTL > 0) $ threadDelaySec $ fromIntegral wTTL
    let rTTL = (toTTL waitOnException, toTTL maxWait)
    srv <- mapConcurrently (collectData rTTL) upstreams
    let (ttl, !newParts) = (minimumTTL rTTL $ map fst srv, map snd srv)
        new = mconcat newParts
    if new == old
        then return ""
        else do
            writeIORef collectedData (ttl, new)
            return $ encode new
    where toTTL = TTL . fromIntegral . toSec

ngxExportSimpleServiceTyped 'collectUpstreams ''Conf $
    PersistentService Nothing

signalUpconf :: Upconf -> Bool -> IO L.ByteString
signalUpconf Upconf {..} = const $ do
    void $ uncurry queryHTTP upconfAddr
    return ""

ngxExportSimpleServiceTyped 'signalUpconf ''Upconf $
    PersistentService Nothing

