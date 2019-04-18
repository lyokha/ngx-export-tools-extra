{-# LANGUAGE TemplateHaskell, DeriveGeneric #-}

module TestToolsExtra where

import           NgxExport.Tools.Aggregate

import           Data.Aeson
import           GHC.Generics

data Stats = Stats { bytesSent :: Int
                   , requests :: Int
                   , meanBytesSent :: Int
                   } deriving Generic
instance FromJSON Stats
instance ToJSON Stats

ngxExportAggregateService "stats" ''Stats

