{-# LANGUAGE TemplateHaskell #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.ServiceHookAdaptor
-- Copyright   :  (c) Alexey Radkov 2021-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-- A service hook adaptor from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.ServiceHookAdaptor (
    -- * Maintaining custom global data in run-time
    -- $maintainingCustomGlobalData
                                          ) where

import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.SplitService
import           NgxExport.Tools.TimeInterval

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Control.Monad

-- $maintainingCustomGlobalData
--
-- This module exports a /simple service/ (in terms of module
-- "NgxExport.Tools.SimpleService") __/simpleService_hookAdaptor/__ which sleeps
-- forever. Its sole purpose is to serve /service hooks/ for changing global
-- data in all the worker processes in run-time. A single service hook adaptor
-- can serve any number of service hooks with any type of global data.
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_servicehookadaptor.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, OverloadedStrings \#-}
--
-- module TestToolsExtraServiceHookAdaptor where
--
-- import           NgxExport
-- import           NgxExport.Tools.ServiceHookAdaptor ()
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
-- import           Data.IORef
-- import           System.IO.Unsafe
--
-- secretWord :: IORef ByteString
-- secretWord = unsafePerformIO $ newIORef ""
-- {-\# NOINLINE secretWord \#-}
--
-- testSecretWord :: ByteString -> IO L.ByteString
-- __/testSecretWord/__ v = do
--     s <- readIORef secretWord
--     return $ if B.null s
--                  then \"null\"
--                  else if v == s
--                           then \"set\"
--                           else \"unset\"
-- 'NgxExport.ngxExportIOYY' 'testSecretWord
--
-- changeSecretWord :: ByteString -> IO L.ByteString
-- __/changeSecretWord/__ s = do
--     writeIORef secretWord s
--     return \"The secret word was changed\"
-- 'NgxExport.ngxExportServiceHook' \'changeSecretWord
-- @
--
-- Here we are going to maintain a /secret word/ of type 'ByteString' in
-- run-time. When a worker process starts, the word is empty. The word can be
-- changed in run-time by triggering service hook /changeSecretWord/. Client
-- requests are managed differently depending on their knowledge of the secret
-- which is tested in handler /testSecretWord/.
--
-- ==== File /nginx.conf/
-- @
-- user                    nobody;
-- worker_processes        2;
--
-- events {
--     worker_connections  1024;
-- }
--
-- error_log               \/tmp\/nginx-test-haskell-error.log info;
--
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--     error_log           \/tmp\/nginx-test-haskell-error.log;
--     access_log          \/tmp\/nginx-test-haskell-access.log;
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_servicehookadaptor.so;
--
--     haskell_run_service __/simpleService_hookAdaptor/__ $hs_hook_adaptor \'\';
--
--     haskell_service_hooks_zone hooks 32k;
--
--     server {
--         listen       8010;
--         server_name  main;
--
--         location \/ {
--             haskell_run __/testSecretWord/__ $hs_secret_word $arg_s;
--
--             if ($hs_secret_word = null) {
--                 echo_status 503;
--                 echo \"Try later! The service is not ready!\";
--                 break;
--             }
--
--             if ($hs_secret_word = set) {
--                 echo_status 200;
--                 echo \"Congrats! You know the secret word!\";
--                 break;
--             }
--
--             echo_status 404;
--             echo \"Hmm, you do not know a secret!\";
--         }
--
--         location \/change_sw {
--             allow 127.0.0.1;
--             deny all;
--
--             haskell_service_hook __/changeSecretWord/__ $hs_hook_adaptor $arg_s;
--         }
--     }
-- }
-- @
--
-- Notice that service /simpleService_hookAdaptor/ is not shared, however this
-- is not such important because shared services must work as well.
--
-- ==== A simple test
-- After starting Nginx, the secret word service must be not ready.
--
-- > $ curl 'http://127.0.0.1:8010/'
-- > Try later! The service is not ready!
--
-- Let's change the secret word,
--
-- > $ curl 'http://127.0.0.1:8010/change_sw?s=secret'
--
-- and try again.
--
-- > $ curl 'http://127.0.0.1:8010/'
-- > Hmm, you do not know a secret!
-- > $ curl 'http://127.0.0.1:8010/?s=try1'
-- > Hmm, you do not know a secret!
-- > $ curl 'http://127.0.0.1:8010/?s=secret'
-- > Congrats! You know the secret word!
--
-- Change the secret word again.
--
-- > $ curl 'http://127.0.0.1:8010/change_sw?s=secret1'
-- > $ curl 'http://127.0.0.1:8010/?s=secret'
-- > Hmm, you do not know a secret!
-- > $ curl 'http://127.0.0.1:8010/?s=secret1'
-- > Congrats! You know the secret word!
--
-- What if a worker process quits for some reason or crashes? Let's try!
--
-- > # ps -ef | grep nginx | grep worker
-- > nobody     13869   13868  0 15:43 ?        00:00:00 nginx: worker process
-- > nobody     13870   13868  0 15:43 ?        00:00:00 nginx: worker process
-- > # kill -QUIT 13869 13870
-- > # ps -ef | grep nginx | grep worker
-- > nobody     14223   13868  4 15:56 ?        00:00:00 nginx: worker process
-- > nobody     14224   13868  4 15:56 ?        00:00:00 nginx: worker process
--
-- > $ curl 'http://127.0.0.1:8010/?s=secret1'
-- > Congrats! You know the secret word!
--
-- Our secret is still intact! This is because service hooks manage new worker
-- processes so well as those that were running when a hook was triggered.

hookAdaptor :: ByteString -> Bool -> IO L.ByteString
hookAdaptor = ignitionService $
    const $ forever $ threadDelaySec $ toSec $ Hr 24

ngxExportSimpleService 'hookAdaptor SingleShotService

