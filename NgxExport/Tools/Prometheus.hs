{-# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE TypeApplications, TupleSections, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Prometheus
-- Copyright   :  (c) Alexey Radkov 2020
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  experimental
-- Portability :  non-portable (requires Template Haskell)
--
-- Prometheus metrics from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Prometheus (
                                   scale
                                  ,scale1000
                                  ) where

import           NgxExport
import           NgxExport.Tools

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Aeson
import           Data.Maybe
import           Data.Word
import           Control.Arrow
import           System.IO.Unsafe
import           GHC.Generics
import           Safe

type ServerName = Text
type MetricsName = Text
type MetricsHelp = Text
type MetricsLabel = Text
type CounterValue = Word64
type MetricsData = Map MetricsName CounterValue
type HistogramData = Map MetricsName (MetricsLabel, Double)
type MetricsToLabelMap = Map MetricsName MetricsLabel

data PrometheusConf =
    PrometheusConf { pcMetrics :: Map MetricsName MetricsHelp
                   , pcScale1000 :: [MetricsName]
                   } deriving Read

data HistogramLayout =
    HistogramLayout { range :: MetricsToLabelMap
                    , cnt :: (MetricsName, MetricsLabel)
                    , err :: (MetricsName, MetricsLabel)
                    } deriving (Generic, Show)

instance FromJSON HistogramLayout

type AllCounters = Map ServerName MetricsData
type AllHistogramsLayout = Map ServerName (Map MetricsName HistogramLayout)
type AllOtherCounters = MetricsData

type AllMetrtics =
    (ServerName, AllCounters, AllHistogramsLayout, AllOtherCounters)

data MetricsType = Counter Double
                 | Histogram HistogramData

type PrometheusMetrics = Map MetricsName (MetricsHelp, MetricsType)

conf :: IORef PrometheusConf
conf = unsafePerformIO $ newIORef $ PrometheusConf M.empty []
{-# NOINLINE conf #-}

prometheusConf :: PrometheusConf -> Bool -> IO L.ByteString
prometheusConf = ignitionService $ \a -> do
    atomicWriteIORef conf a
    return ""

ngxExportSimpleServiceTyped
    'prometheusConf ''PrometheusConf SingleShotService

toPrometheusMetrics' :: AllMetrtics -> IO PrometheusMetrics
toPrometheusMetrics' (srv, cnts, hs, ocnts) = do
    PrometheusConf {..} <- readIORef conf
    let toValues = M.mapWithKey
            (\k v -> (if k `elem` pcScale1000
                          then (/ 1000)
                          else id
                     ) $ fromIntegral v
            )
        cnts' = maybe M.empty toValues $ M.lookup srv cnts
        hs' = M.lookup srv hs
        (cntsH, cntsC) =
            if maybe True M.null hs'
                then (M.empty, M.map Counter cnts')
                else let hs'' = fromJust hs'
                         rs = M.keys &&& M.foldr labeledRange M.empty $ hs''
                         cntsH' = M.filterWithKey (hCounter rs) cnts'
                         cntsC' = cnts' `M.difference` cntsH'
                         cntsH'' = M.mapWithKey
                             (\k -> toHistogram cntsH' k . range) hs''
                     in (M.map Histogram cntsH'', M.map Counter cntsC')
        cntsA = cntsH `M.union` cntsC `M.union` M.map Counter (toValues ocnts)
    return $ M.mapWithKey (\k v -> case M.lookup k pcMetrics of
                                        Nothing -> ("", v)
                                        Just h -> (h, v)
                           ) cntsA
    where labeledRange = M.union . M.filter (not . T.null) . range
          hCounter (ks, ts) k = const $
              k `M.member` ts ||
                  headDef False
                      (dropWhile not $
                          foldr (\v ->
                                     let v1 = v `T.append` "_sum"
                                         v2 = v `T.append` "_cnt"
                                     in ((k == v1 || k == v2) :)
                                ) [] ks
                      )
          toHistogram cs hk rs =
              let ranges = M.mapWithKey
                      (\k l -> case M.lookup k cs of
                                   Just v -> (l, v)
                                   Nothing -> (l, 0.0)
                      ) rs
                  sums = let v1 = hk `T.append` "_sum"
                             v2 = hk `T.append` "_cnt"
                             withZeroLabel = return . ("",)
                         in M.fromList $
                             map (second fromJust) $ filter (isJust . snd)
                                 [(v1, M.lookup v1 cs >>= withZeroLabel)
                                 ,(v2, M.lookup v2 cs >>= withZeroLabel)
                                 ]
              in ranges `M.union` sums

showPrometheusMetrics :: PrometheusMetrics -> L.ByteString
showPrometheusMetrics = L.fromStrict . T.encodeUtf8 . M.foldlWithKey
    (\a k (h, m) -> T.concat [a, "# HELP ", k, " ", h, "\n"
                             ,   "# TYPE ", k, " ", showType m, "\n"
                             ,case m of
                                  Histogram h' -> fst $
                                      M.foldlWithKey (showHistogram k)
                                          ("", 0.0) h'
                                  Counter v ->
                                      T.concat [k, " ", T.pack $ show v, "\n"]
                             ]
    ) ""
    where showType (Counter _) = "counter"
          showType (Histogram _) = "histogram"
          showHistogram k a@(t, n) c (l, v) =
              if T.null l
                  then if k `T.append` "_sum" == c
                           then (T.concat [t, c, " ", T.pack $ show v, "\n"]
                                ,n
                                )
                           else
                                let v' = round v :: Word64
                                in if k `T.append` "_cnt" == c
                                       then (T.concat [t, k, "_count "
                                                      ,T.pack $ show v', "\n"
                                                      ]
                                            ,n
                                            )
                                        else a
                  else let n' = n + v
                       in (T.concat [t, k, "_bucket{le=\"", l, "\"} "
                                    ,T.pack $ show (round n' :: Word64), "\n"
                                    ]
                          ,n'
                          )

toPrometheusMetrics :: ByteString -> IO L.ByteString
toPrometheusMetrics v = do
    let v' = fromJust $ readFromByteStringAsJSON @AllMetrtics v
    showPrometheusMetrics <$> toPrometheusMetrics' v'

ngxExportIOYY 'toPrometheusMetrics

scale :: Int -> Double -> Int
scale n = round . (fromIntegral n *)

scale1000 :: ByteString -> L.ByteString
scale1000 v = let v' = fromJust $ readFromByteString @Double v
              in C8L.pack $ show $ scale 1000 v'

ngxExportYY 'scale1000

