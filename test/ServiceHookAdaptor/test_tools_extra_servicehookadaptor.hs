{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module TestToolsExtraServiceHookAdaptor where

import           NgxExport
import           NgxExport.Tools.ServiceHookAdaptor ()

import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.ByteString.Lazy (LazyByteString)
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

testSecretWord :: ByteString -> IO LazyByteString
testSecretWord v = do
    s <- readIORef secretWord
    when (B.null s) $ throwIO SecretWordUnset
    return $ if v == s
                 then "success"
                 else ""
ngxExportIOYY 'testSecretWord

changeSecretWord :: ByteString -> IO LazyByteString
changeSecretWord s = do
    writeIORef secretWord s
    return $ "The secret word was " `L.append` if B.null s
                                                   then "reset"
                                                   else "changed"
ngxExportServiceHook 'changeSecretWord

