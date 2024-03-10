{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module TestToolsExtraServiceHookAdaptor where

import           NgxExport
import           NgxExport.Tools.ServiceHookAdaptor ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           Control.Monad
import           Control.Exception
import           System.IO.Unsafe

data SecretWordUnset = SecretWordUnset

instance Exception SecretWordUnset
instance Show SecretWordUnset where
    show = const "unset"

secretWord :: IORef ByteString
secretWord = unsafePerformIO $ newIORef ""
{-# NOINLINE secretWord #-}

testSecretWord :: ByteString -> IO L.ByteString
testSecretWord v = do
    s <- readIORef secretWord
    when (B.null s) $ throwIO SecretWordUnset
    return $ if v == s
                 then "success"
                 else ""
ngxExportIOYY 'testSecretWord

changeSecretWord :: ByteString -> IO L.ByteString
changeSecretWord s = do
    writeIORef secretWord s
    return $ L.concat ["The secret word was "
                      ,if B.null s
                           then "reset"
                           else "changed"
                      ]
ngxExportServiceHook 'changeSecretWord

