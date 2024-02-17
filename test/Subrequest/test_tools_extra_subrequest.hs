{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module TestToolsExtraSubrequest where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Subrequest

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import           Network.HTTP.Client
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SB
import qualified Data.ByteString.Char8 as C8

makeRequest :: ByteString -> Bool -> IO L.ByteString
makeRequest = const . makeSubrequest

ngxExportSimpleService 'makeRequest $ PersistentService $ Just $ Sec 10

reqBody :: L.ByteString -> ByteString -> IO L.ByteString
reqBody = const . return

ngxExportAsyncOnReqBody 'reqBody

configureUdsManager :: ByteString -> Bool -> IO L.ByteString
configureUdsManager = ignitionService $ \path -> voidHandler $ do
    man <- newManager defaultManagerSettings
               { managerRawConnection = return $ openUDS path }
    registerCustomManager "myuds" man
    where openUDS path _ _ _ = do
              s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
              S.connect s (S.SockAddrUnix $ C8.unpack path)
              makeConnection (SB.recv s 4096) (SB.sendAll s) (S.close s)

ngxExportSimpleService 'configureUdsManager SingleShotService

