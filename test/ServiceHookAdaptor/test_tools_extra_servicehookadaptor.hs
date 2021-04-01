{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module TestToolsExtraServiceHookAdaptor where

import           NgxExport
import           NgxExport.Tools.ServiceHookAdaptor ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           System.IO.Unsafe

secretWord :: IORef ByteString
secretWord = unsafePerformIO $ newIORef ""
{-# NOINLINE secretWord #-}

testSecretWord :: ByteString -> IO L.ByteString
testSecretWord v = do
    s <- readIORef secretWord
    return $ if v == s
                 then "1"
                 else ""
ngxExportIOYY 'testSecretWord

changeSecretWord :: ByteString -> IO L.ByteString
changeSecretWord s = do
    writeIORef secretWord s
    return "The secret word was changed"
ngxExportServiceHook 'changeSecretWord

