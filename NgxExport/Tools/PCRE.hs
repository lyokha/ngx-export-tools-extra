{-# LANGUAGE TemplateHaskell, BangPatterns, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.PCRE
-- Copyright   :  (c) Alexey Radkov 2021-2024
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-- PCRE matching and substitution from the more extra tools collection
-- for <https://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport.Tools.PCRE (
    -- * Matching against regular expressions
    -- $matchingPCRE
                             matchRegex
    -- * Substitution with regular expressions
    -- $substitutionPCRE
                            ,SubPasteF
                            ,subRegex
                            ,subRegexWith
                            ,gsubRegex
                            ,gsubRegexWith
                            ) where

import           NgxExport
import           NgxExport.Tools.Combinators
import           NgxExport.Tools.SimpleService

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.List
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.IORef
import           Text.Regex.PCRE.Light
import           Text.Regex.PCRE.Heavy
import           Control.Exception (Exception, throwIO)
import           Control.Arrow
import           System.IO.Unsafe

-- $matchingPCRE
--
-- This module provides a simple handler __/matchRegex/__ to match a value
-- against a PCRE regex preliminary declared and compiled in
-- /configuration service/ __/simpleService_declareRegexes/__ (which is an
-- 'ignitionService' in terms of module "NgxExport.Tools.SplitService") and the
-- corresponding /service update hook/ (in terms of module "NgxExport")
-- __/compileRegexes/__ at the start of the service.
--
-- Below is a simple example.
--
-- ==== File /test_tools_extra_pcre.hs/
-- @
-- module TestToolsExtraPCRE where
--
-- import NgxExport.Tools.PCRE ()
-- @
--
-- The file does not contain any significant declarations as we are going to use
-- only the exporters of the handlers.
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
-- http {
--     default_type        application\/octet-stream;
--     sendfile            on;
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_pcre.so;
--
--     haskell_run_service __/simpleService_declareRegexes/__ $hs_regexes
--             \'[(\"__/userArea/__\", \"(?:\\\\\\\\|)(\\\\\\\\d+)$\", \"\")
--              ,(\"__/keyValue/__\", \"(k\\\\\\\\w+)(\\\\\\\\|)(v\\\\\\\\w+)\", \"i\")
--              ]\';
--
--     haskell_service_update_hook __/compileRegexes/__ $hs_regexes;
--
--     server {
--         listen       8010;
--         server_name  main;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         location \/ {
--             haskell_run __/matchRegex/__ $hs_user_area '__/userArea|/__$arg_user';
--             rewrite ^ \/internal\/user\/area\/$hs_user_area last;
--         }
--
--         location ~ ^\/internal\/user\/area\/(PCRE\ ERROR:.*) {
--             internal;
--             echo_status 404;
--             echo \"Bad input: $1\";
--         }
--
--         location = \/internal\/user\/area\/ {
--             internal;
--             echo_status 404;
--             echo \"No user area attached\";
--         }
--
--         location ~ ^\/internal\/user\/area\/(.+) {
--             internal;
--             echo \"User area: $1\";
--         }
--     }
-- }
-- @
--
-- In this example, we expect requests with argument /user/ which should
-- supposedly be tagged with an /area/ code containing digits only. The /user/
-- value should match against regex /userArea/ declared alongside with another
-- regex /keyValue/ (the latter has an option /i/ which corresponds to
-- 'caseless'; the regex compiler has also support for options /s/ and /m/ which
-- correspond to 'dotall' and 'multiline' respectively). Notice that regex
-- declarations require 4-fold backslashes as they are getting shrunk while
-- interpreted sequentially by the Nginx configuration interpreter and then by
-- the Haskell compiler too.
--
-- Handler /matchRegex/ finds the named regex /userArea/ from the beginning of
-- its argument: the second part of the argument is delimited by a /bar/ symbol
-- and contains the value to match against. If the regex contains captures, then
-- the matched value shall correspond to the contents of the first capture (in
-- case of /userArea/, this is the area code), otherwise it must correspond to
-- the whole matched value.
--
-- ==== A simple test
--
-- > $ curl 'http://localhost:8010/'
-- > No user area attached
-- > $ curl 'http://localhost:8010/?user=peter|98'
-- > User area: 98
-- > $ curl 'http://localhost:8010/?user=peter|98i'
-- > No user area attached

type InputRegexes = [(ByteString, ByteString, ByteString)]
type Regexes = HashMap ByteString Regex

newtype MatchRegexError = MatchRegexError String

instance Exception MatchRegexError
instance Show MatchRegexError where
    show (MatchRegexError s) = "PCRE ERROR: " ++ s

regexes :: IORef Regexes
regexes = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE regexes #-}

declareRegexes :: InputRegexes -> NgxExportService
declareRegexes = voidService

ngxExportSimpleServiceTyped 'declareRegexes ''InputRegexes restartPromptly

compileRegexes :: ByteString -> IO L.ByteString
compileRegexes = voidHandler' $ do
    !inputRegexes <- fromJust <$> readIORef storage_InputRegexes_declareRegexes
    let !compiledRegexes =
            foldl' (\a (!k, !v, !m) -> let !r = compile v $ mods $ C8.unpack m
                                           !hm = HM.insert k r a
                                       in hm
                   ) HM.empty inputRegexes
    writeIORef regexes compiledRegexes
    where md 'i' = Just caseless
          md 's' = Just dotall
          md 'm' = Just multiline
          md  _  = Nothing
          mods = map NE.head . NE.group . sort . mapMaybe md

ngxExportServiceHook 'compileRegexes

type InputSubs = [(ByteString, ByteString)]
type Subs = HashMap ByteString ByteString

substitutions :: IORef Subs
substitutions = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE substitutions #-}

mapSubs :: InputSubs -> NgxExportService
mapSubs = ignitionService $ voidHandler .
    writeIORef substitutions . foldl (\a (k, v) -> HM.insert k v a) HM.empty

ngxExportSimpleServiceTyped 'mapSubs ''InputSubs SingleShotService

type RegexF = Regex -> ByteString -> IO ByteString

rtRegex :: RegexF -> ByteString -> IO L.ByteString
rtRegex f = fmap L.fromStrict . uncurry doRtRegex .
    second C8.tail . C8.break (== '|')
    where doRtRegex k v = do
              rgxs <- readIORef regexes
              case HM.lookup k rgxs of
                  Nothing -> throwIO $ MatchRegexError $
                      "Regex " ++ C8.unpack k ++ " was not found"
                  Just r -> f r v

doMatchRegex :: RegexF
doMatchRegex r v = return $
    case match r v [] of
        Just (_ : c1 : _) -> c1
        Just (c0 : _) -> c0
        _ -> ""

-- | Matches a value against a named regex.
--
-- The regex must be preliminary declared and compiled by service handlers
-- /simpleService_declareRegexes/ and /compileRegexes/. The name of the regex
-- and the value are passed in a single argument: the two parts are delimited by
-- the first /bar/ symbol met from the left, e.g. /key|value/.
--
-- This is the core function of the /matchRegex/ handler.
matchRegex
    :: ByteString           -- ^ Key to find the regex, and the value
    -> IO L.ByteString
matchRegex = rtRegex doMatchRegex

ngxExportIOYY 'matchRegex

-- $substitutionPCRE
--
-- There are handlers to make substitutions using PCRE regexes. An
-- 'ignitionService' __/simpleService_mapSubs/__ declares named /plain/
-- substitutions which are made in run-time by handlers __/subRegex/__ and
-- __/gsubRegex/__. Functions 'subRegexWith' and 'gsubRegexWith' make it
-- possible to write custom /functional/ substitutions.
--
-- Let's extend our example by adding ability to erase the captured area code.
-- We also going to implement a /functional/ substitution to swap the keys and
-- the values matched in the /keyValue/ regex.
--
-- ==== File /test_tools_extra_pcre.hs/
-- @
-- {-\# LANGUAGE TemplateHaskell, LambdaCase \#-}
--
-- module TestToolsExtraPCRE where
--
-- import           NgxExport
-- import           NgxExport.Tools.PCRE
--
-- import           Data.ByteString (ByteString)
-- import qualified Data.ByteString as B
-- import qualified Data.ByteString.Lazy as L
--
-- gsubSwapAround :: ByteString -> IO L.ByteString
-- __/gsubSwapAround/__ = 'gsubRegexWith' $ const $ \\case 
--     a : d : b : _ -> B.concat [b, d, a]
--     \_ -> B.empty
--
-- 'ngxExportIOYY' \'gsubSwapAround
-- @
--
-- Functional substitution handler /gsubSwapAround/ expects a regular expression
-- with at least 3 capture groups to swap the contents of the first and the
-- third groups around. We are going to apply this handler against regex
-- /keyValue/.
--
-- ==== File /nginx.conf/: erase area code and swap keys and values
-- @
--     haskell_run_service __/simpleService_mapSubs/__ $hs_subs
--             \'[(\"__/erase/__\", \"\")]\';
--
--     haskell_var_empty_on_error $hs_kv;
-- @
-- @
--         location \/erase\/area {
--             haskell_run __/subRegex/__ $hs_user_no_area \'__/userArea|erase|/__$arg_user\';
--             rewrite ^ \/internal\/user\/noarea\/$hs_user_no_area last;
--         }
--
--         location ~ ^\/internal\/user\/noarea\/(PCRE\\ ERROR:.*) {
--             internal;
--             echo_status 404;
--             echo \"Bad input: $1\";
--         }
--
--         location ~ ^\/internal\/user\/noarea\/(.*) {
--             internal;
--             echo \"User without area: $1\";
--         }
--
--         location \/swap {
--             haskell_run __/gsubSwapAround/__ $hs_kv \'__/keyValue|/__$arg_kv\';
--             echo \"Swap $arg_kv = $hs_kv\";
--         }
-- @
--
-- Service /simpleService_mapSubs/ declares a list of named /plain/
-- substitutions. In this example, it declares only one substitution /erase/
-- which substitutes an empty string, i.e. /erases/ the matched text. Notice
-- that the argument of handler /subRequest/ requires three parts delimited by
-- /bar/ symbols: the named regex, the named substitution, and the value to
-- match against.
--
-- ==== A simple test
--
-- > $ curl 'http://localhost:8010/erase/area?user=peter|98'
-- > User without area: peter
-- > $ curl 'http://localhost:8010/swap?kv=kid|v0012a'
-- > Swap kid|v0012a = v0012a|kid

-- | Type of functions to perform /functional/ substitutions.
type SubPasteF =
       ByteString       -- ^ The full match
    -> [ByteString]     -- ^ List of captures
    -> ByteString

type SubF = Regex -> SubPasteF -> ByteString -> ByteString

doSubRegex :: SubF -> Maybe SubPasteF -> RegexF
doSubRegex f p r v =
    case p of
        Nothing -> do
            let (k, v') = second C8.tail $ C8.break (== '|') v
            subs <- readIORef substitutions
            case HM.lookup k subs of
                Nothing -> throwIO $ MatchRegexError $
                    "Substitution " ++ C8.unpack k ++ " was not found"
                Just s -> return $ f r (const . const s) v'
        Just paste -> return $ f r paste v

-- | Pastes a named /plain/ substitution using a named regex.
--
-- The substitution and the regex must be preliminary declared and compiled by
-- service handlers /simpleService_declareRegexes/, /compileRegexes/, and
-- /simpleService_mapSubs/. The names of the regex and the substitution, and
-- the value are passed in a single argument: the three parts are delimited by
-- /bar/ symbols, e.g. /regex|sub|value/. The substitution gets applied only to
-- the first occurrence of the match.
--
-- This is the core function of the /subRegex/ handler.
subRegex
    :: ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
subRegex = rtRegex $ doSubRegex sub Nothing

ngxExportIOYY 'subRegex

-- | Pastes /functional/ substitutions using a named regex and a function.
--
-- The substitutions get applied only to the first occurrence of the match.
subRegexWith
    :: SubPasteF        -- ^ Function to paste substitutions
    -> ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
subRegexWith = rtRegex . doSubRegex sub . Just

-- | Pastes a named /plain/ substitution using a named regex.
--
-- The same as 'subRegex' except that the substitution gets applied
-- globally, wherever the match occurs.
--
-- This is the core function of the /gsubRegex/ handler.
gsubRegex
    :: ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
gsubRegex = rtRegex $ doSubRegex gsub Nothing

ngxExportIOYY 'gsubRegex

-- | Pastes /functional/ substitutions using a named regex and a function.
--
-- The same as 'subRegexWith' except that the substitutions get applied
-- globally, wherever the match occurs.
gsubRegexWith
    :: SubPasteF        -- ^ Function to paste substitutions
    -> ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
gsubRegexWith = rtRegex . doSubRegex gsub . Just

