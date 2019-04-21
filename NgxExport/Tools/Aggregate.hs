{-# LANGUAGE TemplateHaskell, OverloadedStrings, BangPatterns #-}

module NgxExport.Tools.Aggregate (
    -- * The typed service exporter
                                  AggregateServerConf
                                 ,ngxExportAggregateService
    -- * The client-side reporter
                                 ,reportAggregate
    -- * Re-exported data constructors from /Foreign.C/
    -- | Re-exports are needed by the exporter for marshalling in foreign calls.
                                 ,Foreign.C.Types.CInt (..)
                                 ,Foreign.C.Types.CUInt (..)
                                 ) where

import           NgxExport.Tools

import           Language.Haskell.TH
import           Network.HTTP.Client
import           Foreign.C.Types
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.IORef
import           Data.Int
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           Data.Aeson
import           Data.Maybe
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Arrow
import           Control.Exception
import           Control.Exception.Enclosed (handleAny)
import           System.IO.Unsafe
import           Snap.Http.Server
import           Snap.Core

type Aggregate a = IORef (CTime, Map Int32 (CTime, Maybe a))

data AggregateServerConf =
    AggregateServerConf { asPort          :: Int
                        , asPurgeInterval :: TimeInterval
                        } deriving Read

aggregateServer :: (FromJSON a, ToJSON a) =>
    Aggregate a -> ByteString -> AggregateServerConf -> Bool -> IO L.ByteString
aggregateServer a u = ignitionService $ \conf ->
    simpleHttpServe (asConfig $ asPort conf) (asHandler a u conf) >> return ""

asConfig :: Int -> Config Snap a
asConfig p = setPort p
           $ setBind "127.0.0.1"
           $ setAccessLog ConfigNoLog
           $ setErrorLog ConfigNoLog
           $ setVerbose False mempty

asHandler :: (FromJSON a, ToJSON a) =>
    Aggregate a -> ByteString -> AggregateServerConf -> Snap ()
asHandler a u conf =
    route [(B.append "put/" u, Snap.Core.method POST $ receiveAggregate a conf)
          ,(B.append "get/" u, Snap.Core.method GET $ sendAggregate a)
          ]

receiveAggregate :: FromJSON a =>
    Aggregate a -> AggregateServerConf -> Snap ()
receiveAggregate a conf =
    handleAggregateExceptions "Exception while receiving aggregate" $ do
        !s <- decode' <$> readRequestBody 65536
        when (isNothing s) $ liftIO $ throwUserError "Unreadable aggregate!"
        liftIO $ do
            let (pid, v) = fromJust s
                int = fromIntegral . toSec . asPurgeInterval $ conf
            !t <- ngxNow
            atomicModifyIORef' a $
                \(t', v') ->
                    (let (!tn, f) =
                             if t - t' >= int
                                 then (t, M.filter $ \(t'', _) -> t - t'' < int)
                                 else (t', id)
                         !vn = f $ M.alter
                                   (\old ->
                                       let !new' =
                                               if isNothing old || isJust v
                                                   then v
                                                   else snd $ fromJust old
                                       in Just (t, new')
                                   ) pid v'
                     in (tn, vn)
                    ,()
                    )
        finishWith emptyResponse

sendAggregate :: ToJSON a => Aggregate a -> Snap ()
sendAggregate a =
    handleAggregateExceptions "Exception while sending aggregate" $ do
        s <- liftIO $ readIORef a
        modifyResponse $ setContentType "application/json"
        writeLBS $ encode $ M.map (first $ \(CTime t) ->
                                      posixSecondsToUTCTime $ fromIntegral t
                                  ) $ snd s

handleAggregateExceptions :: String -> Snap () -> Snap ()
handleAggregateExceptions cmsg = handleAny $ \e ->
    writeErrorResponse 500 $ show (e :: SomeException)
    where writeErrorResponse c msg = do
              modifyResponse $ setResponseStatus c $ T.encodeUtf8 $ T.pack cmsg
              writeBS $ T.encodeUtf8 $ T.pack msg

throwUserError :: String -> IO a
throwUserError = ioError . userError

httpManager :: Manager
httpManager = unsafePerformIO $ newManager defaultManagerSettings
{-# NOINLINE httpManager #-}

reportAggregate :: ToJSON a => Int -> Maybe a -> ByteString -> IO ()
reportAggregate p v u =
    handle (const $ return () :: SomeException -> IO ()) $ do
        req <- parseRequest "POST http://127.0.0.1"
        pid <- fromIntegral <$> ngxPid :: IO Int32
        let !req' = req { requestBody = RequestBodyLBS $ encode (pid, v)
                        , port = p
                        , Network.HTTP.Client.path = B.append "put/" u
                        }
        void $ httpNoBody req' httpManager

ngxExportAggregateService :: String       -- ^ Name of the service
                          -> Name         -- ^ Name of the aggregate type
                          -> Q [Dec]
ngxExportAggregateService f a = do
    let nameF = 'aggregateServer
        fName = mkName $ "aggregate_" ++ f
        sName = mkName $ "aggregate_storage_" ++ f
        uName = mkName $ "aggregate_url_" ++ f
    concat <$> sequence
        [sequence
            [sigD uName [t|ByteString|]
            ,funD uName [clause [] (normalB [|C8.pack f|]) []]
            ,sigD sName [t|Aggregate $(conT a)|]
            ,funD sName
                [clause []
                    (normalB [|unsafePerformIO $ newIORef (0, M.empty)|])
                    []
                ]
            ,pragInlD sName NoInline FunLike AllPhases
            ,sigD fName [t|AggregateServerConf -> Bool -> IO L.ByteString|]
            ,funD fName
                [clause []
                    (normalB [|$(varE nameF) $(varE sName) $(varE uName)|])
                    []
                ]
            ]
        -- FIXME: name AggregateServerConf must be imported from the user's
        -- module unqualified (see details in NgxExport/Tools.hs, function
        -- ngxExportSimpleService')!
        ,ngxExportSimpleServiceTyped
            fName ''AggregateServerConf SingleShotService
        ]

