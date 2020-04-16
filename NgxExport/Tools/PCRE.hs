{-# LANGUAGE TemplateHaskell, BangPatterns, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.PCRE
-- Copyright   :  (c) Alexey Radkov 2020
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- PCRE parsing and substitution from the more extra tools collection
-- for <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------

module NgxExport.Tools.PCRE (
    -- * Parsing and substitution of PCRE-styled regular expressions
    -- $parsingAndSubstitutionPCRE
                             matchRegex
                            ) where

import           NgxExport
import           NgxExport.Tools

import qualified Data.HashMap.Strict as HM
import           Data.HashMap.Strict (HashMap)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.List
import           Data.Maybe
import           Data.IORef
import           Text.Regex.PCRE.Light
import           Text.Regex.PCRE.Heavy
import           Control.Exception (Exception, throwIO)
import           Control.Arrow
import           System.IO.Unsafe

-- $parsingAndSubstitutionPCRE
--

type InputRegexes = [(ByteString, ByteString, ByteString)]
type Regexes = HashMap ByteString Regex

newtype MatchRegexError = MatchRegexError String

instance Exception MatchRegexError
instance Show MatchRegexError where
    show (MatchRegexError s) = "PCRE ERROR: " ++ s

regexes :: IORef Regexes
regexes = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE regexes #-}

compileRegexes :: InputRegexes -> Bool -> IO L.ByteString
compileRegexes = ignitionService $ \irgxs -> do
    writeIORef regexes $
        foldl (\a (k, v, m) -> let !r = compile v $ mods $ C8.unpack m
                               in HM.insert k r a
              ) HM.empty irgxs
    return ""
    where md 'i' = Just caseless
          md 's' = Just dotall
          md 'm' = Just multiline
          md  _  = Nothing
          mods = map head . group . sort . mapMaybe md

ngxExportSimpleServiceTyped 'compileRegexes ''InputRegexes SingleShotService

type InputSubs = [(ByteString, ByteString)]
type Subs = HashMap ByteString ByteString

substitutions :: IORef Subs
substitutions = unsafePerformIO $ newIORef HM.empty
{-# NOINLINE substitutions #-}

declareSubs :: InputSubs -> Bool -> IO L.ByteString
declareSubs = ignitionService $ \isubs -> do
    writeIORef substitutions $
        foldl (\a (k, v) -> HM.insert k v a) HM.empty isubs
    return ""

ngxExportSimpleServiceTyped 'declareSubs ''InputSubs SingleShotService

type RegexF = Regex -> ByteString -> IO ByteString
type SubConvF = ByteString -> ByteString
type SubF = Regex -> SubConvF -> ByteString -> ByteString 

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
        Nothing -> ""
        Just cs -> if captureCount r == 0
                       then head cs
                       else head $ tail cs

matchRegex
    :: ByteString           -- ^ Key to find the regex, and the value
    -> IO L.ByteString
matchRegex = rtRegex doMatchRegex

ngxExportIOYY 'matchRegex

doSubRegex :: SubF -> Maybe SubConvF -> RegexF
doSubRegex f c r = uncurry doSubRegex' . second C8.tail . C8.break (== '|')
    where doSubRegex' :: ByteString -> ByteString -> IO ByteString
          doSubRegex' k v = do
              subs <- readIORef substitutions
              case HM.lookup k subs of
                  Nothing -> throwIO $ MatchRegexError $
                      "Substitution " ++ C8.unpack k ++ " was not found"
                  Just s -> return $
                      case c of
                          Nothing -> f r (const s) v
                          Just conv -> f r conv v

subRegex
    :: ByteString       -- ^ Keys to find the regex and the subst, and the value
    -> IO L.ByteString
subRegex = rtRegex $ doSubRegex sub Nothing

ngxExportIOYY 'subRegex

gsubRegex
    :: ByteString       -- ^ Keys to find the regex and the subst, and the value
    -> IO L.ByteString
gsubRegex = rtRegex $ doSubRegex gsub Nothing

ngxExportIOYY 'gsubRegex

