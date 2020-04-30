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
    -- * Matching of regular expressions
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

-- $matchingPCRE
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

mapSubs :: InputSubs -> Bool -> IO L.ByteString
mapSubs = ignitionService $ \isubs -> do
    writeIORef substitutions $
        foldl (\a (k, v) -> HM.insert k v a) HM.empty isubs
    return ""

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
        Nothing -> ""
        Just cs -> if captureCount r == 0
                       then head cs
                       else head $ tail cs

matchRegex
    :: ByteString           -- ^ Key to find the regex, and the value
    -> IO L.ByteString
matchRegex = rtRegex doMatchRegex

ngxExportIOYY 'matchRegex

-- $substitutionPCRE
--

-- | Type of functions to paste captures into a substitution.
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

subRegex
    :: ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
subRegex = rtRegex $ doSubRegex sub Nothing

ngxExportIOYY 'subRegex

subRegexWith
    :: SubPasteF        -- ^ Function to paste substitutions
    -> ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
subRegexWith = rtRegex . doSubRegex sub . Just

gsubRegex
    :: ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
gsubRegex = rtRegex $ doSubRegex gsub Nothing

ngxExportIOYY 'gsubRegex

gsubRegexWith
    :: SubPasteF        -- ^ Function to paste substitutions
    -> ByteString       -- ^ Keys to find the regex and the sub, and the value
    -> IO L.ByteString
gsubRegexWith = rtRegex . doSubRegex gsub . Just

