{-# LANGUAGE TemplateHaskell, DeriveGeneric, RecordWildCards #-}
{-# LANGUAGE TypeApplications, TupleSections, OverloadedStrings #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  NgxExport.Tools.Prometheus
-- Copyright   :  (c) Alexey Radkov 2020-2022
-- License     :  BSD-style
--
-- Maintainer  :  alexey.radkov@gmail.com
-- Stability   :  stable
-- Portability :  non-portable (requires Template Haskell)
--
-- Prometheus metrics from the more extra tools collection for
-- <http://github.com/lyokha/nginx-haskell-module nginx-haskell-module>.
--
-----------------------------------------------------------------------------


module NgxExport.Tools.Prometheus (
    -- * Exporters
    -- $exporters

    -- * Utilities
    -- *** Scaling functions
                                   scale
                                  ,scale1000
    -- *** Converting lists of values to counters
    -- $convertingListsOfValuesToCounters

    -- * Parameterization of metrics with custom labels
    -- $parameterization
                                  ) where

import           NgxExport
import           NgxExport.Tools.Read
import           NgxExport.Tools.SimpleService
import           NgxExport.Tools.SplitService

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.IORef
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import           Data.Aeson
import           Data.Maybe
import           Data.Function
import           Data.List
import           Data.Char
import           Data.Word
import           Data.Array.ST hiding (range)
import           Control.Arrow
import           Control.Monad
import           Control.Monad.ST
import           System.IO.Unsafe
import           GHC.Generics
import           Safe

type ServerName = Text
type MetricsName = Text
type MetricsHelp = Text
type MetricsLabel = Text
type MetricsAnnotation = Text
type CounterValue = Word64
type MetricsData = Map MetricsName CounterValue
type HistogramData = Map MetricsName (MetricsLabel, MetricsRole, Double)
type MetricsToLabelMap = Map MetricsName MetricsLabel

data PrometheusConf =
    PrometheusConf { pcMetrics :: Map MetricsName MetricsHelp
                   , pcGauges :: HashSet MetricsName
                   , pcScale1000 :: HashSet MetricsName
                   } deriving Read

data HistogramLayout =
    HistogramLayout { range :: MetricsToLabelMap
                    , cnt :: (MetricsName, MetricsLabel)
                    , err :: (MetricsName, MetricsLabel)
                    } deriving Generic

instance FromJSON HistogramLayout

type AllCounters = Map ServerName MetricsData
type AllHistogramsLayout = Map ServerName (Map MetricsName HistogramLayout)
type AllOtherCounters = MetricsData

type AllMetrtics =
    (ServerName, AllCounters, AllHistogramsLayout, AllOtherCounters)

data MetricsType = Counter Double MetricsAnnotation
                 | Gauge Double MetricsAnnotation
                 | Histogram HistogramData MetricsAnnotation

data MetricsRole = HistogramBucket
                 | HistogramSum
                 | HistogramCount deriving Eq

type PrometheusMetrics = Map MetricsName (MetricsHelp, [MetricsType])

conf :: IORef (Maybe PrometheusConf)
conf = unsafePerformIO $ newIORef Nothing
{-# NOINLINE conf #-}

-- $exporters
--
-- This module is aimed to convert custom counters from
-- [nginx-custom-counters-module](https://github.com/lyokha/nginx-custom-counters-module)
-- to Prometheus metrics. For this, it exposes four exporters:
-- __/prometheusConf/__ which is an 'ignitionService' in terms of module
-- "NgxExport.Tools", __/toPrometheusMetrics/__ to convert /custom counters/ to
-- Prometheus metrics, __/prometheusMetrics/__ which is a content handler aiming
-- to return Prometheus metrics to the client, and a handy utility
-- __/scale1000/__ to convert small floating point numbers to integers by
-- multiplying them by /1000/ (which fits well for dealing with request
-- durations).
--
-- The module makes use of a few custom data types which are not exported while
-- still needed when writing Nginx configurations. In the following example they
-- are used in configurations of /simpleService_prometheusConf/ and
-- /toPrometheusMetrics/.
--
-- ==== File /test_tools_extra_prometheus.hs/
-- @
-- module TestToolsExtraPrometheus where
--
-- import NgxExport.Tools.Prometheus ()
-- @
--
-- The file does not contain any significant declarations as we are going to use
-- only the exporters.
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
--     map $status $inc_cnt_4xx {
--         default         0;
--         \'~^4(?:\\d){2}\'  1;
--     }
--
--     map $status $inc_cnt_5xx {
--         default         0;
--         \'~^5(?:\\d){2}\'  1;
--     }
--
--     map_to_range_index $hs_request_time $request_time_bucket
--         0.005
--         0.01
--         0.05
--         0.1
--         0.5
--         1.0
--         5.0
--         10.0
--         30.0
--         60.0;
--
--     map_to_range_index $hs_bytes_sent $bytes_sent_bucket
--         0
--         10
--         100
--         1000
--         10000;
--
--     haskell load \/var\/lib\/nginx\/test_tools_extra_prometheus.so;
--
--     haskell_run_service __/simpleService_prometheusConf/__ $hs_prometheus_conf
--             \'__/PrometheusConf/__
--                 { __/pcMetrics/__ = fromList
--                     [(\"cnt_4xx\", \"Number of responses with 4xx status\")
--                     ,(\"cnt_5xx\", \"Number of responses with 5xx status\")
--                     ,(\"cnt_stub_status_active\", \"Active requests\")
--                     ,(\"cnt_uptime\", \"Nginx master uptime\")
--                     ,(\"cnt_uptime_reload\", \"Nginx master uptime after reload\")
--                     ,(\"hst_request_time\", \"Request duration\")
--                     ]
--                 , __/pcGauges/__ = fromList
--                     [\"cnt_stub_status_active\"]
--                 , __/pcScale1000/__ = fromList
--                     [\"hst_request_time_sum\"]
--                 }';
--
--     haskell_var_empty_on_error $hs_prom_metrics;
--
--     counters_survive_reload on;
--
--     server {
--         listen       8010;
--         server_name  __/main/__;
--         error_log    \/tmp\/nginx-test-haskell-error.log;
--         access_log   \/tmp\/nginx-test-haskell-access.log;
--
--         counter $cnt_4xx inc $inc_cnt_4xx;
--         counter $cnt_5xx inc $inc_cnt_5xx;
--
--         \# cache $request_time and $bytes_sent
--         haskell_run ! $hs_request_time $request_time;
--         haskell_run ! $hs_bytes_sent $bytes_sent;
--
--         histogram $hst_request_time 11 $request_time_bucket;
--         haskell_run __/scale1000/__ $hs_request_time_scaled $hs_request_time;
--         counter $hst_request_time_sum inc $hs_request_time_scaled;
--
--         histogram $hst_bytes_sent 6 $bytes_sent_bucket;
--         counter $hst_bytes_sent_sum inc $hs_bytes_sent;
--
--         location \/ {
--             echo_sleep 0.5;
--             echo Ok;
--         }
--
--         location \/1 {
--             echo_sleep 1.0;
--             echo Ok;
--         }
--
--         location \/404 {
--             return 404;
--         }
--     }
--
--     server {
--         listen       8020;
--         server_name  stats;
--
--         location \/ {
--             haskell_run __/toPrometheusMetrics/__ $hs_prom_metrics
--                     '[\"__/main/__\"
--                      ,$__/cnt_collection/__
--                      ,$__/cnt_histograms/__
--                      ,{\"cnt_stub_status_active\": $cnt_stub_status_active
--                       ,\"cnt_uptime\": $cnt_uptime
--                       ,\"cnt_uptime_reload\": $cnt_uptime_reload
--                       }
--                      ]\';
--
--             if ($hs_prom_metrics = \'\') {
--                 return 503;
--             }
--
--             default_type \"text/plain; version=0.0.4; charset=utf-8\";
--
--             echo -n $hs_prom_metrics;
--         }
--
--         location \/counters {
--             default_type application/json;
--             echo $cnt_collection;
--         }
--
--         location \/histograms {
--             default_type application/json;
--             echo $cnt_histograms;
--         }
--
--         location \/uptime {
--             echo "Uptime (after reload): $cnt_uptime ($cnt_uptime_reload)";
--         }
--     }
-- }
-- @
--
-- Type /PrometheusConf/ contains fields /pcMetrics/, /pcGauges/, and
-- /pcScale1000/. Field /pcMetrics/ is a map from metrics names to help
-- messages: this can be used to bind small descriptions to the metrics as
-- /nginx-custom-counters-module/ does not provide such functionality. Setting
-- descriptions to counters is optional. Field /pcGauges/ lists counters that
-- must be regarded as gauges: the number of currently active requests is
-- obviously a gauge. Field /pcScale1000/ contains a list of counters that were
-- scaled with /scale1000/ and must be converted back.
--
-- Handler /toPrometheusMetrics/ expects 4 fields: the name of the
-- /counter set identifier/ &#8212; in our example there is only one counter
-- set /main/, predefined variables /cnt_collection/ and /cnt_histograms/ from
-- /nginx-custom-counters-module/, and a list of additional counters &#8212; in
-- our example there are three additional counters /cnt_stub_status_active/,
-- /cnt_uptime/, and /cnt_uptime_reload/ which are also defined in
-- /nginx-custom-counters-module/.
--
-- To fulfill histogram description in Prometheus, the /sum/ value must be
-- provided. Histogram sums are not supported in /nginx-custom-counters-module/,
-- and therefore they must be declared in separate counters. In this example
-- there are two histograms collecting request durations and the number of sent
-- bytes, and accordingly, there are two sum counters: /hst_request_time_sum/
-- and /hst_bytes_sent_sum/. As request durations may last milliseconds while
-- being shown in seconds, they must be scaled with /scale1000/.
--
-- To further ensure histogram validity, it is important to have the last bucket
-- in a histogram labeled as /\"+Inf\"/. This is achieved automatically when
-- the number of range boundaries in directive /map_to_range_index/ is less by
-- one than the number in the corresponding histogram declaration: in this
-- example, the map for /request_time_bucket/ has 10 range boundaries while
-- histogram /hst_request_time/ has 11 buckets, the map for /bytes_sent_bucket/
-- has 5 range boundaries while histogram /hst_bytes_sent/ has 6 buckets.
--
-- Notice that the variable handler /toPrometheusMetrics/ and directive /echo/
-- in location /\// can be replaced with a single content handler
-- /prometheusMetrics/ like in the following block.
--
-- @
--         location \/ {
--             haskell_async_content __/prometheusMetrics/__
--                     '[\"__/main/__\"
--                      ,$__/cnt_collection/__
--                      ,$__/cnt_histograms/__
--                      ,{\"cnt_stub_status_active\": $cnt_stub_status_active
--                       ,\"cnt_uptime\": $cnt_uptime
--                       ,\"cnt_uptime_reload\": $cnt_uptime_reload
--                       }
--                      ]\';
--         }
-- @
--
-- ==== A simple test
--
-- Let's look at the metrics right after starting Nginx.
--
-- > $ curl -s 'http://localhost:8020/'
-- > # HELP cnt_4xx Number of responses with 4xx status
-- > # TYPE cnt_4xx counter
-- > cnt_4xx 0.0
-- > # HELP cnt_5xx Number of responses with 5xx status
-- > # TYPE cnt_5xx counter
-- > cnt_5xx 0.0
-- > # HELP cnt_stub_status_active Active requests
-- > # TYPE cnt_stub_status_active gauge
-- > cnt_stub_status_active 1.0
-- > # HELP cnt_uptime Nginx master uptime
-- > # TYPE cnt_uptime counter
-- > cnt_uptime 8.0
-- > # HELP cnt_uptime_reload Nginx master uptime after reload
-- > # TYPE cnt_uptime_reload counter
-- > cnt_uptime_reload 8.0
-- > # HELP hst_bytes_sent 
-- > # TYPE hst_bytes_sent histogram
-- > hst_bytes_sent_bucket{le="0"} 0
-- > hst_bytes_sent_bucket{le="10"} 0
-- > hst_bytes_sent_bucket{le="100"} 0
-- > hst_bytes_sent_bucket{le="1000"} 0
-- > hst_bytes_sent_bucket{le="10000"} 0
-- > hst_bytes_sent_bucket{le="+Inf"} 0
-- > hst_bytes_sent_count 0
-- > hst_bytes_sent_sum 0.0
-- > # HELP hst_bytes_sent_err 
-- > # TYPE hst_bytes_sent_err counter
-- > hst_bytes_sent_err 0.0
-- > # HELP hst_request_time Request duration
-- > # TYPE hst_request_time histogram
-- > hst_request_time_bucket{le="0.005"} 0
-- > hst_request_time_bucket{le="0.01"} 0
-- > hst_request_time_bucket{le="0.05"} 0
-- > hst_request_time_bucket{le="0.1"} 0
-- > hst_request_time_bucket{le="0.5"} 0
-- > hst_request_time_bucket{le="1.0"} 0
-- > hst_request_time_bucket{le="5.0"} 0
-- > hst_request_time_bucket{le="10.0"} 0
-- > hst_request_time_bucket{le="30.0"} 0
-- > hst_request_time_bucket{le="60.0"} 0
-- > hst_request_time_bucket{le="+Inf"} 0
-- > hst_request_time_count 0
-- > hst_request_time_sum 0.0
-- > # HELP hst_request_time_err 
-- > # TYPE hst_request_time_err counter
-- > hst_request_time_err 0.0
--
-- Run some requests and look at the metrics again.
--
-- > $ for i in {1..20} ; do curl -D- 'http://localhost:8010/' & done
-- >   ...
-- > $ for i in {1..30} ; do curl -D- 'http://localhost:8010/1' & done
-- >   ...
-- > $ curl 'http://127.0.0.1:8010/404'
-- >   ...
--
-- > $ curl -s 'http://localhost:8020/'
-- > # HELP cnt_4xx Number of responses with 4xx status
-- > # TYPE cnt_4xx counter
-- > cnt_4xx 1.0
-- > # HELP cnt_5xx Number of responses with 5xx status
-- > # TYPE cnt_5xx counter
-- > cnt_5xx 0.0
-- > # HELP cnt_stub_status_active Active requests
-- > # TYPE cnt_stub_status_active gauge
-- > cnt_stub_status_active 1.0
-- > # HELP cnt_uptime Nginx master uptime
-- > # TYPE cnt_uptime counter
-- > cnt_uptime 371.0
-- > # HELP cnt_uptime_reload Nginx master uptime after reload
-- > # TYPE cnt_uptime_reload counter
-- > cnt_uptime_reload 371.0
-- > # HELP hst_bytes_sent 
-- > # TYPE hst_bytes_sent histogram
-- > hst_bytes_sent_bucket{le="0"} 0
-- > hst_bytes_sent_bucket{le="10"} 0
-- > hst_bytes_sent_bucket{le="100"} 0
-- > hst_bytes_sent_bucket{le="1000"} 51
-- > hst_bytes_sent_bucket{le="10000"} 51
-- > hst_bytes_sent_bucket{le="+Inf"} 51
-- > hst_bytes_sent_count 51
-- > hst_bytes_sent_sum 9458.0
-- > # HELP hst_bytes_sent_err 
-- > # TYPE hst_bytes_sent_err counter
-- > hst_bytes_sent_err 0.0
-- > # HELP hst_request_time Request duration
-- > # TYPE hst_request_time histogram
-- > hst_request_time_bucket{le="0.005"} 1
-- > hst_request_time_bucket{le="0.01"} 1
-- > hst_request_time_bucket{le="0.05"} 1
-- > hst_request_time_bucket{le="0.1"} 1
-- > hst_request_time_bucket{le="0.5"} 13
-- > hst_request_time_bucket{le="1.0"} 44
-- > hst_request_time_bucket{le="5.0"} 51
-- > hst_request_time_bucket{le="10.0"} 51
-- > hst_request_time_bucket{le="30.0"} 51
-- > hst_request_time_bucket{le="60.0"} 51
-- > hst_request_time_bucket{le="+Inf"} 51
-- > hst_request_time_count 51
-- > hst_request_time_sum 40.006
-- > # HELP hst_request_time_err 
-- > # TYPE hst_request_time_err counter
-- > hst_request_time_err 0.0

prometheusConf :: PrometheusConf -> Bool -> IO L.ByteString
prometheusConf = ignitionService $ \a -> do
    writeIORef conf $ Just a
    return ""

ngxExportSimpleServiceTyped 'prometheusConf ''PrometheusConf SingleShotService

toPrometheusMetrics' :: PrometheusConf -> AllMetrtics -> PrometheusMetrics
toPrometheusMetrics' PrometheusConf {..} (srv, cnts, hs, ocnts) =
    let toValues = M.mapWithKey
            (\k v -> (if k `HS.member` pcScale1000
                          then (/ 1000)
                          else id
                     ) $ fromIntegral v
            )
        cnts' = maybe M.empty toValues $ M.lookup srv cnts
        hs' = M.lookup srv hs
        (cntsH, cntsC, cntsG) =
            if maybe True M.null hs'
                then let (cntsG', cntsC') = M.partitionWithKey gCounter cnts'
                     in (M.empty, cntsC', cntsG')
                else let hs'' = fromJust hs'
                         rs = M.keys &&& M.foldr labeledRange M.empty $ hs''
                         (cntsH', (cntsG', cntsC')) =
                             second (M.partitionWithKey gCounter) $
                             M.partitionWithKey (hCounter rs) cnts'
                         cntsH'' = M.mapWithKey
                             (\k -> toHistogram cntsH' k . range) hs''
                     in (cntsH'', cntsC', cntsG')
        (cntsOG, cntsOC) = M.partitionWithKey gCounter $ toValues ocnts
        cntsA = collect Histogram id cntsH
                `M.union` collect Counter renameErrCounter
                    (cntsC `M.union` cntsOC)
                `M.union` collect Gauge id (cntsG `M.union` cntsOG)
    in M.mapWithKey (\k -> (fromMaybe "" (M.lookup k pcMetrics),)) cntsA
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
          gCounter = const . flip HS.member pcGauges
          toHistogram cs hk rs =
              let ranges = M.mapWithKey
                      (\k ->
                          (, HistogramBucket, fromMaybe 0.0 (M.lookup k cs))
                      ) rs
                  sums = let v1 = hk `T.append` "_sum"
                             v2 = hk `T.append` "_cnt"
                             withZeroLabel r = return . ("", r,)
                         in M.fromList $
                             map (second fromJust) $ filter (isJust . snd)
                                 [(v1, M.lookup v1 cs >>=
                                     withZeroLabel HistogramSum
                                  )
                                 ,(v2, M.lookup v2 cs >>=
                                     withZeroLabel HistogramCount
                                  )
                                 ]
              in ranges `M.union` sums
          collect cType renameErrCounterF =
              M.fromAscList
              . map ((fst . head) &&& map (uncurry cType . snd))
              . groupBy ((==) `on` fst)
              . map (\(k, v) ->
                        let (k', a) =
                                second (\a' ->
                                            if T.null a'
                                                then ""
                                                else T.map
                                                     (\c ->
                                                         if c == '(' || c == ')'
                                                             then '"'
                                                             else c
                                                     ) $ T.tail a'
                                       ) $ T.breakOn "@" k
                        in (k', (v, a))
                    )
              . M.toList
              . M.mapKeys renameErrCounterF
          renameErrCounter k =
              let s = "_err"
                  (b, (a, e)) =
                      second (\v -> maybe (v, "") (, s) $ T.stripSuffix s v) $
                          T.breakOn "@" k
              in T.concat [b, e, a]

showPrometheusMetrics :: PrometheusMetrics -> L.ByteString
showPrometheusMetrics = TL.encodeUtf8 . M.foldlWithKey
    (\a k (h, ms) ->
        let k' = TL.fromStrict k
        in TL.concat [a, "# HELP ", k', " ", TL.fromStrict h, "\n"
                     ,   "# TYPE ", k', " ", showType $ head ms, "\n"
                     ,foldl (showCounter k') "" ms
                     ]
    ) ""
    where showType (Counter _ _) = "counter"
          showType (Gauge _ _) = "gauge"
          showType (Histogram _ _) = "histogram"
          showCounter k a m =
              TL.concat [a
                        ,case m of
                            Counter v anno -> TL.concat
                                [k, showAnno anno, " ", TL.pack $ show v, "\n"]
                            Gauge v anno -> TL.concat
                                [k, showAnno anno, " ", TL.pack $ show v, "\n"]
                            Histogram h' anno -> fst $
                                M.foldlWithKey (showHistogram k anno)
                                    ("", 0.0) h'
                        ]
          showAnno x = let x' = TL.fromStrict x
                       in if TL.null x'
                              then x'
                              else TL.concat ["{", x', "}"]
          showAnnoH x = let x' = TL.fromStrict x
                        in if TL.null x'
                               then x'
                               else TL.concat [",", x']
          showHistogram k anno a@(t, n) _ (l, r, v) =
              if T.null l
                  then case r of
                      HistogramSum ->
                          (TL.concat [t, k, "_sum"
                                     ,showAnno anno, " "
                                     ,TL.pack $ show v
                                     ,"\n"
                                     ]
                          ,n
                          )
                      HistogramCount ->
                          (TL.concat [t, k, "_count"
                                     ,showAnno anno, " "
                                     ,TL.pack $ show (round v :: Word64)
                                     ,"\n"
                                     ]
                          ,n
                          )
                      _  -> a
                  else let n' = n + v
                       in (TL.concat [t, k
                                     ,"_bucket{le=\"", TL.fromStrict l, "\""
                                     ,showAnnoH anno, "} "
                                     ,TL.pack $ show (round n' :: Word64)
                                     ,"\n"
                                     ]
                          ,n'
                          )

toPrometheusMetrics :: ByteString -> IO L.ByteString
toPrometheusMetrics v = do
    let cs = fromJust $ readFromByteStringAsJSON @AllMetrtics v
    pc <- readIORef conf
    return $ maybe "" (showPrometheusMetrics . flip toPrometheusMetrics' cs) pc

ngxExportIOYY 'toPrometheusMetrics

textPlain :: ByteString
textPlain = "text/plain; version=0.0.4; charset=utf-8"

prometheusMetrics :: ByteString -> IO ContentHandlerResult
prometheusMetrics = fmap (, textPlain, 200, []) . toPrometheusMetrics

ngxExportAsyncHandler 'prometheusMetrics

-- | Multiplies a floating point value by a factor.
--
-- Returns an integer value as the result of rounding the scaled floating point
-- value.
scale
    :: Int          -- ^ Factor
    -> Double       -- ^ Floating point value
    -> Int
scale n = round . (fromIntegral n *)

-- | Multiplies a floating point value by /1000/.
--
-- The floating point value gets read from a 'ByteString'. Throws an exception
-- on conversion failure which results in returning an empty string.
scale1000
    :: ByteString   -- ^ Floating point value
    -> L.ByteString
scale1000 v = let v' = fromJust $ readFromByteString @Double v
              in C8L.pack $ show $ scale 1000 v'

ngxExportYY 'scale1000

-- $convertingListsOfValuesToCounters
--
-- This module has limited support for extracting data from lists of values.
-- Normally, variables from Nginx upstream module such as /upstream_status/,
-- /upstream_response_time/ and others contain lists of values separated by
-- commas and semicolons. With handler __/statusLayout/__, numbers of /2xx/,
-- /3xx/, /4xx/ and /5xx/ responses from backends can be collected in a
-- comma-separated list. Handlers __/cumulativeValue/__ and
-- __/cumulativeFPValue/__ can be used to count cumulative integer and floating
-- point numbers from lists of values.
--
-- Let's add checking upstream statuses and cumulative response times from all
-- servers in an upstream into the original file /nginx.conf/ from the previous
-- example.
--
-- ==== File /nginx.conf/: checking upstream statuses and response times
-- @
--     upstream backends {
--         server 127.0.0.1:8030 max_fails=0;
--         server 127.0.0.1:8040 max_fails=0;
--     }
-- @
-- @
--     server {
--         listen       8030;
--         server_name  backend1;
--
--         location \/ {
--             echo_sleep 0.5;
--             echo_status 404;
--             echo \"Backend1 Ok\";
--         }
--     }
--
--     server {
--         listen       8040;
--         server_name  backend2;
--
--         location \/ {
--             echo_status 504;
--             echo \"Backend2 Ok\";
--         }
--     }
-- @
--
-- Here we added upstream /backends/ with two virtual servers that will play
-- the role of backends. One of them will wait for half a second and return
-- HTTP status /404/, while the other will return HTTP status /504/ immediately.
-- Both servers are tagged with /max_fails=0/ to prevent blacklisting them.
--
-- We also have to add counters and mappings.
--
-- @
--     map $hs_upstream_status $inc_cnt_u_4xx {
--         default                               0;
--         \'~^(?:(?:\\d+),){2}(?P\<m_status\>\\d+)\'  $m_status;
--     }
--
--     map $hs_upstream_status $inc_cnt_u_5xx {
--         default                               0;
--         \'~^(?:(?:\\d+),){3}(?P\<m_status\>\\d+)\'  $m_status;
--     }
--
--     map_to_range_index $hs_u_response_time $u_response_time_bucket
--         0.005
--         0.01
--         0.05
--         0.1
--         0.5
--         1.0
--         5.0
--         10.0
--         30.0
--         60.0;
-- @
-- @
--         haskell_run __/statusLayout/__ $hs_upstream_status $upstream_status;
--         counter $__/cnt_u_4xx/__ inc $inc_cnt_u_4xx;
--         counter $__/cnt_u_5xx/__ inc $inc_cnt_u_5xx;
--
--         \# cache $upstream_response_time
--         haskell_run ! $hs_u_response_times $upstream_response_time;
--
--         histogram $__/hst_u_response_time/__ 11 $u_response_time_bucket;
--         histogram $__/hst_u_response_time/__ undo;
--         haskell_run __/cumulativeFPValue/__ $hs_u_response_time $hs_u_response_times;
--         haskell_run __/scale1000/__ $hs_u_response_time_scaled $hs_u_response_time;
-- @
--
-- Notice that histogram /hst_u_response_time/ was disabled on this level to
-- not count visiting unrelated locations (i.e. /\//, /\/1/, and /\/404/): the
-- histogram will be re-enabled later in locations related to proxying requests.
-- The sum counter will also be declared inside the proxying locations and take
-- the value of /hs_u_response_time_scaled/ as the input value.
--
-- So many new variables require a bigger hash table to store them.
--
-- @
--     variables_hash_max_size 4096;
-- @
--
-- And finally, we have to update counters declarations in
-- /simpleService_prometheusConf/ and add location /\/backends/ in the main
-- server.
--
-- @
--     haskell_run_service __/simpleService_prometheusConf/__ $hs_prometheus_conf
--             \'__/PrometheusConf/__
--                 { __/pcMetrics/__ = fromList
--                     [(\"cnt_4xx\", \"Number of responses with 4xx status\")
--                     ,(\"cnt_5xx\", \"Number of responses with 5xx status\")
--                     ,(\"__/cnt_u_4xx/__\"
--                      ,\"Number of responses from upstreams with 4xx status\")
--                     ,(\"__/cnt_u_5xx/__\"
--                      ,\"Number of responses from upstreams with 5xx status\")
--                     ,(\"cnt_stub_status_active\", \"Active requests\")
--                     ,(\"cnt_uptime\", \"Nginx master uptime\")
--                     ,(\"cnt_uptime_reload\", \"Nginx master uptime after reload\")
--                     ,(\"hst_request_time\", \"Request duration\")
--                     ,(\"__/hst_u_response_time/__\"
--                      ,\"Response time from all servers in a single upstream\")
--                     ]
--                 , __/pcGauges/__ = fromList
--                     [\"cnt_stub_status_active\"]
--                 , __/pcScale1000/__ = fromList
--                     [\"hst_request_time_sum\"
--                     ,\"__/hst_u_response_time_sum/__\"
--                     ]
--                 }\';
-- @
-- @
--         location \/backends {
--             histogram $__/hst_u_response_time/__ reuse;
--             counter $__/hst_u_response_time_sum/__ inc $hs_u_response_time_scaled;
--             error_page 404 \@status404;
--             proxy_intercept_errors on;
--             proxy_pass http:\/\/backends;
--         }
--
--         location \@status404 {
--             histogram $__/hst_u_response_time/__ reuse;
--             counter $__/hst_u_response_time_sum/__ inc $hs_u_response_time_scaled;
--             echo_sleep 0.2;
--             echo \"Caught 404\";
--         }
-- @
--
-- We are going to additionally increase response time by /0.2/ seconds when a
-- backend server responds with HTTP status /404/, and this is why location
-- /\@status404/ was added.
--
-- ==== A simple test
--
-- After restart of Nginx.
--
-- > $ for i in {1..20} ; do curl -D- 'http://localhost:8010/backends' & done
-- >   ...
--
-- > $ curl -s 'http://127.0.0.1:8020/'
-- > # HELP cnt_4xx Number of responses with 4xx status
-- > # TYPE cnt_4xx counter
-- > cnt_4xx 11.0
-- > # HELP cnt_5xx Number of responses with 5xx status
-- > # TYPE cnt_5xx counter
-- > cnt_5xx 9.0
-- > # HELP cnt_stub_status_active Active requests
-- > # TYPE cnt_stub_status_active gauge
-- > cnt_stub_status_active 1.0
-- > # HELP cnt_u_4xx Number of responses from upstreams with 4xx status
-- > # TYPE cnt_u_4xx counter
-- > cnt_u_4xx 11.0
-- > # HELP cnt_u_5xx Number of responses from upstreams with 5xx status
-- > # TYPE cnt_u_5xx counter
-- > cnt_u_5xx 9.0
-- > # HELP cnt_uptime Nginx master uptime
-- > # TYPE cnt_uptime counter
-- > cnt_uptime 63.0
-- > # HELP cnt_uptime_reload Nginx master uptime after reload
-- > # TYPE cnt_uptime_reload counter
-- > cnt_uptime_reload 63.0
-- > # HELP hst_bytes_sent
-- > # TYPE hst_bytes_sent histogram
-- > hst_bytes_sent_bucket{le="0"} 0
-- > hst_bytes_sent_bucket{le="10"} 0
-- > hst_bytes_sent_bucket{le="100"} 0
-- > hst_bytes_sent_bucket{le="1000"} 20
-- > hst_bytes_sent_bucket{le="10000"} 20
-- > hst_bytes_sent_bucket{le="+Inf"} 20
-- > hst_bytes_sent_count 20
-- > hst_bytes_sent_sum 4032.0
-- > # HELP hst_bytes_sent_err
-- > # TYPE hst_bytes_sent_err counter
-- > hst_bytes_sent_err 0.0
-- > # HELP hst_request_time Request duration
-- > # TYPE hst_request_time histogram
-- > hst_request_time_bucket{le="0.005"} 9
-- > hst_request_time_bucket{le="0.01"} 9
-- > hst_request_time_bucket{le="0.05"} 9
-- > hst_request_time_bucket{le="0.1"} 9
-- > hst_request_time_bucket{le="0.5"} 9
-- > hst_request_time_bucket{le="1.0"} 20
-- > hst_request_time_bucket{le="5.0"} 20
-- > hst_request_time_bucket{le="10.0"} 20
-- > hst_request_time_bucket{le="30.0"} 20
-- > hst_request_time_bucket{le="60.0"} 20
-- > hst_request_time_bucket{le="+Inf"} 20
-- > hst_request_time_count 20
-- > hst_request_time_sum 7.721
-- > # HELP hst_request_time_err
-- > # TYPE hst_request_time_err counter
-- > hst_request_time_err 0.0
-- > # HELP hst_u_response_time Response time from all servers in a single upstream
-- > # TYPE hst_u_response_time histogram
-- > hst_u_response_time_bucket{le="0.005"} 9
-- > hst_u_response_time_bucket{le="0.01"} 9
-- > hst_u_response_time_bucket{le="0.05"} 9
-- > hst_u_response_time_bucket{le="0.1"} 9
-- > hst_u_response_time_bucket{le="0.5"} 13
-- > hst_u_response_time_bucket{le="1.0"} 20
-- > hst_u_response_time_bucket{le="5.0"} 20
-- > hst_u_response_time_bucket{le="10.0"} 20
-- > hst_u_response_time_bucket{le="30.0"} 20
-- > hst_u_response_time_bucket{le="60.0"} 20
-- > hst_u_response_time_bucket{le="+Inf"} 20
-- > hst_u_response_time_count 20
-- > hst_u_response_time_sum 5.519
-- > # HELP hst_u_response_time_err
-- > # TYPE hst_u_response_time_err counter
-- > hst_u_response_time_err 0.0
--
-- Counters look good. Numbers of visiting backend servers are almost equal (11
-- and 9), the sum of cumulative response times from backends is approximately 5
-- seconds, while the sum of all requests durations is approximately 7 seconds
-- which corresponds to 11 visits to location /\@status404/ and the sleep time
-- /0.2/ seconds that was added there.

extractValues :: ByteString -> [ByteString]
extractValues = filter ((&&) <$> not . C8.null <*> isDigit . C8.head)
                . C8.splitWith ((&&) <$> not . isDigit <*> (/= '.'))

statusLayout :: ByteString -> L.ByteString
statusLayout = C8L.pack . intercalate "," . map show . statuses
    where statuses s = runST $ do
              a <- newArray bs 0 :: ST s (STUArray s Int Int)
              mapM_ (uncurry $ writeStatus a) $ toPairs s
              getElems a
          toPairs = map (subtract (ord '0') . ord . C8.head . head &&& length)
                    . groupBy ((==) `on` C8.head)
                    . sort
                    . extractValues
          writeStatus a i = when (i >= lb && i <= ub) . writeArray a i
          bs@(lb, ub) = (2, 5)

ngxExportYY 'statusLayout

cumulativeValue' :: (Num a, Read a) => ByteString -> a
cumulativeValue' = foldr ((+) . (read . C8.unpack)) 0 . extractValues

cumulativeValue :: ByteString -> L.ByteString
cumulativeValue = C8L.pack . show . cumulativeValue' @Int

ngxExportYY 'cumulativeValue

cumulativeFPValue :: ByteString -> L.ByteString
cumulativeFPValue = C8L.pack . show . cumulativeValue' @Double

ngxExportYY 'cumulativeFPValue

-- $parameterization
--
-- In the previous examples we used many counters which served similar purposes.
-- For example, counters /cnt_4xx/, /cnt_5xx/, /cnt_u_4xx/, and /cnt_u_5xx/
-- counted response statuses in different conditions: particularly, the 2 former
-- counters counted /4xx/ and /5xx/ response statuses sent to clients, while the
-- latter 2 counters counted /4xx/ and /5xx/ response statuses received from the
-- upstream. It feels that they could be shown as a single compound counter
-- parameterized by the range of values and the origin. We also had two
-- histograms /hst_request_time/ and /hst_u_response_time/ which could also be
-- combined in a single entity parameterized by the scope (the time of the whole
-- request against the time spent in the upstream).
--
-- Fortunately, Prometheus provides a mechanism to make such custom
-- parameterizations by using /labels/ in metrics. This module supports the
-- parameterization with labels by expecting special /annotations/ attached to
-- the names of the counters.
--
-- Let's parameterize the status counters and the request times as it was
-- proposed at the beginning of this section.
--
-- ==== File /nginx.conf/: changes related to counters annotations
-- @
--     haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
--             \'PrometheusConf
--                 { pcMetrics = fromList
--                     [(\"__/cnt_status/__\", \"Number of responses with given status\")
--                     ,(\"cnt_stub_status_active\", \"Active requests\")
--                     ,(\"cnt_uptime\", \"Nginx master uptime\")
--                     ,(\"cnt_uptime_reload\", \"Nginx master uptime after reload\")
--                     ,(\"__/hst_request_time/__\", \"Request duration\")
--                     ]
--                 , pcGauges = fromList
--                     [\"cnt_stub_status_active\"]
--                 , pcScale1000 = fromList
--                     [\"__/hst_request_time@\scope=(total)_sum/__\"
--                     ,\"__/hst_request_time\@scope=(in_upstreams)_sum/__\"
--                     ]
--                 }\';
--
-- @
-- @
--         counter $__/cnt_status\@value=(4xx),from=(response)/__ inc $inc_cnt_4xx;
--         counter $__/cnt_status\@value=(5xx),from=(response)/__ inc $inc_cnt_5xx;
--
--         haskell_run statusLayout $hs_upstream_status $upstream_status;
--         counter $__/cnt_status\@value=(4xx),from=(upstream)/__ inc $inc_cnt_u_4xx;
--         counter $__/cnt_status\@value=(5xx),from=(upstream)/__ inc $inc_cnt_u_5xx;
--
--         # cache $request_time and $bytes_sent
--         haskell_run ! $hs_request_time $request_time;
--         haskell_run ! $hs_bytes_sent $bytes_sent;
--
--         histogram $__/hst_request_time\@scope=(total)/__ 11 $request_time_bucket;
--         haskell_run scale1000 $hs_request_time_scaled $hs_request_time;
--         counter $hst_request_time\@scope=(total)_sum inc $hs_request_time_scaled;
--
--         histogram $hst_bytes_sent 6 $bytes_sent_bucket;
--         counter $hst_bytes_sent_sum inc $hs_bytes_sent;
--
--         # cache $upstream_response_time
--         haskell_run ! $hs_u_response_times $upstream_response_time;
--
--         histogram $__/hst_request_time\@scope=(in_upstreams)/__ 11
--                 $u_response_time_bucket;
--         histogram $__/hst_request_time\@scope=(in_upstreams)/__ undo;
--         haskell_run cumulativeFPValue $hs_u_response_time $hs_u_response_times;
--         haskell_run scale1000 $hs_u_response_time_scaled $hs_u_response_time;
--
--         location \/ {
--             echo_sleep 0.5;
--             echo Ok;
--         }
--
--         location \/1 {
--             echo_sleep 1.0;
--             echo Ok;
--         }
--
--         location \/404 {
--             return 404;
--         }
--
--         location \/backends {
--             histogram $__/hst_request_time\@scope=(in_upstreams)/__ reuse;
--             counter $__/hst_request_time\@scope=(in_upstreams)_sum/__ inc
--                     $hs_u_response_time_scaled;
--             error_page 404 \@status404;
--             proxy_intercept_errors on;
--             proxy_pass http:\/\/backends;
--         }
--
--         location \@status404 {
--             histogram $__/hst_request_time\@scope=(in_upstreams)/__ reuse;
--             counter $__/hst_request_time\@scope=(in_upstreams)_sum/__ inc
--                     $hs_u_response_time_scaled;
--             echo_sleep 0.2;
--             echo \"Caught 404\";
--         }
-- @
--
-- Notice that the 4 status counters were combined into a compound counter
-- /cnt_status/ whose name was annotated by a tail starting with /\@/. This
-- annotation gets put in the list of labels of the Prometheus metrics with
-- symbols /(/ and /)/ replaced by /\"/ without any further validation. The
-- request time histograms and the corresponding sum counters were annotated in
-- a similar way. Annotations in histogram sum counters must be put between the
-- base name of the counter and the suffix /_sum/.
--
-- ==== A simple test
--
-- > $ curl 'http://127.0.0.1:8010/404'
-- >   ...
-- > $ for i in {1..20} ; do curl -D- 'http://localhost:8010/backends' & done
-- >   ...
--
-- > $ curl -s 'http://localhost:8020/'
-- > # HELP cnt_status Number of responses with given status
-- > # TYPE cnt_status counter
-- > cnt_status{value="4xx",from="response"} 11.0
-- > cnt_status{value="4xx",from="upstream"} 10.0
-- > cnt_status{value="5xx",from="response"} 10.0
-- > cnt_status{value="5xx",from="upstream"} 10.0
-- > # HELP cnt_stub_status_active Active requests
-- > # TYPE cnt_stub_status_active gauge
-- > cnt_stub_status_active 1.0
-- > # HELP cnt_uptime Nginx master uptime
-- > # TYPE cnt_uptime counter
-- > cnt_uptime 70.0
-- > # HELP cnt_uptime_reload Nginx master uptime after reload
-- > # TYPE cnt_uptime_reload counter
-- > cnt_uptime_reload 70.0
-- > # HELP hst_bytes_sent 
-- > # TYPE hst_bytes_sent histogram
-- > hst_bytes_sent_bucket{le="0"} 0
-- > hst_bytes_sent_bucket{le="10"} 0
-- > hst_bytes_sent_bucket{le="100"} 0
-- > hst_bytes_sent_bucket{le="1000"} 21
-- > hst_bytes_sent_bucket{le="10000"} 21
-- > hst_bytes_sent_bucket{le="+Inf"} 21
-- > hst_bytes_sent_count 21
-- > hst_bytes_sent_sum 4348.0
-- > # HELP hst_bytes_sent_err 
-- > # TYPE hst_bytes_sent_err counter
-- > hst_bytes_sent_err 0.0
-- > # HELP hst_request_time Request duration
-- > # TYPE hst_request_time histogram
-- > hst_request_time_bucket{le="0.005",scope="in_upstreams"} 10
-- > hst_request_time_bucket{le="0.01",scope="in_upstreams"} 10
-- > hst_request_time_bucket{le="0.05",scope="in_upstreams"} 10
-- > hst_request_time_bucket{le="0.1",scope="in_upstreams"} 10
-- > hst_request_time_bucket{le="0.5",scope="in_upstreams"} 14
-- > hst_request_time_bucket{le="1.0",scope="in_upstreams"} 20
-- > hst_request_time_bucket{le="5.0",scope="in_upstreams"} 20
-- > hst_request_time_bucket{le="10.0",scope="in_upstreams"} 20
-- > hst_request_time_bucket{le="30.0",scope="in_upstreams"} 20
-- > hst_request_time_bucket{le="60.0",scope="in_upstreams"} 20
-- > hst_request_time_bucket{le="+Inf",scope="in_upstreams"} 20
-- > hst_request_time_count{scope="in_upstreams"} 20
-- > hst_request_time_sum{scope="in_upstreams"} 5.012
-- > hst_request_time_bucket{le="0.005",scope="total"} 11
-- > hst_request_time_bucket{le="0.01",scope="total"} 11
-- > hst_request_time_bucket{le="0.05",scope="total"} 11
-- > hst_request_time_bucket{le="0.1",scope="total"} 11
-- > hst_request_time_bucket{le="0.5",scope="total"} 11
-- > hst_request_time_bucket{le="1.0",scope="total"} 21
-- > hst_request_time_bucket{le="5.0",scope="total"} 21
-- > hst_request_time_bucket{le="10.0",scope="total"} 21
-- > hst_request_time_bucket{le="30.0",scope="total"} 21
-- > hst_request_time_bucket{le="60.0",scope="total"} 21
-- > hst_request_time_bucket{le="+Inf",scope="total"} 21
-- > hst_request_time_count{scope="total"} 21
-- > hst_request_time_sum{scope="total"} 7.02
-- > # HELP hst_request_time_err 
-- > # TYPE hst_request_time_err counter
-- > hst_request_time_err{scope="in_upstreams"} 0.0
-- > hst_request_time_err{scope="total"} 0.0

