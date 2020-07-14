More extra tools for Nginx Haskell module
=========================================

[![Build Status](https://travis-ci.com/lyokha/ngx-export-tools-extra.svg?branch=master)](https://travis-ci.org/lyokha/ngx-export-tools-extra)
[![Hackage](https://img.shields.io/hackage/v/ngx-export-tools-extra.svg?label=hackage%20%7C%20ngx-export-tools-extra)](https://hackage.haskell.org/package/ngx-export-tools-extra)

This package contains a collection of Haskell modules with more extra tools for
[*Nginx Haskell module*](http://github.com/lyokha/nginx-haskell-module).

#### Table of contents

- [Module NgxExport.Tools.Aggregate](#module-ngxexporttoolsaggregate)
- [Module NgxExport.Tools.EDE](#module-ngxexporttoolsede)
- [Module NgxExport.Tools.Prometheus](#module-ngxexporttoolsprometheus)
- [Module NgxExport.Tools.Subrequest](#module-ngxexporttoolssubrequest)
- [Building and installation](#building-and-installation) 

#### Module *NgxExport.Tools.Aggregate*

An aggregate service collects custom typed data reported by worker processes
and sends this via HTTP when requested. This is an *ignitionService* in terms
of module *NgxExport.Tools*, which means that it starts upon the startup of
the worker process and runs until termination of the worker. Internally, an
aggregate service starts an HTTP server implemented via the [*Snap
framework*](http://snapframework.com/), which serves incoming requests from
worker processes (collecting data) as well as from the Nginx server's
clients (reporting collected data for administration purpose).

See detailed documentation on the module's exported functions and data on the
[*Hackage
haddocks*](http://hackage.haskell.org/package/ngx-export-tools-extra/docs/NgxExport-Tools-Aggregate.html).

##### An example

###### File *test_tools_extra_aggregate.hs*

```haskell
{-# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module TestToolsExtraAggregate where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Aggregate

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.Maybe
import           Data.IORef
import           System.IO.Unsafe
import           GHC.Generics

data Stats = Stats { bytesSent :: Int
                   , requests :: Int
                   , meanBytesSent :: Int
                   } deriving Generic
instance FromJSON Stats
instance ToJSON Stats

stats :: IORef Stats
stats = unsafePerformIO $ newIORef $ Stats 0 0 0
{-# NOINLINE stats #-}

updateStats :: ByteString -> IO C8L.ByteString
updateStats s = do
    let cbs = readFromByteString @Int s
    modifyIORef' stats $ \(Stats bs rs _) ->
        let !nbs = bs + fromMaybe 0 cbs
            !nrs = rs + 1
            !nmbs = nbs `div` nrs
        in Stats nbs nrs nmbs
    return ""
ngxExportIOYY 'updateStats

reportStats :: Int -> Bool -> IO C8L.ByteString
reportStats = deferredService $ \port -> do
    s <- readIORef stats
    reportAggregate port (Just s) "stats"
    return ""
ngxExportSimpleServiceTyped 'reportStats ''Int $
    PersistentService $ Just $ Sec 5

ngxExportAggregateService "stats" ''Stats
```

Here, on the bottom line, aggregate service *stats* is declared. It expects
from worker processes reports in JSON format with data of type *Stats* which
includes the number of bytes sent so far, the number of client requests, and
the mean value of bytes sent per a single request. Its own configuration
(a TCP port and the *purge interval*) shall be defined in the Nginx
configuration file. The reports from worker processes are sent from a
*deferredService* *reportStats* every 5 seconds: it merely reads data
collected in a global IORef *stats* and then sends this to the aggregate
service using *reportAggregate*. Handler *updateStats* updates the *stats*
on every run. It accepts a *ByteString* from Nginx, then converts it to an
*Int* value and interprets this as the number of bytes sent in the current
request. It also increments the number or requests and calculates the mean
value of bytes sent in all requests to this worker so far. Notice that all
the parts of *stats* are evaluated *strictly*, it is important!

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    haskell load /var/lib/nginx/test_tools_extra_aggregate.so;

    haskell_run_service simpleService_aggregate_stats $hs_stats
            'AggregateServerConf { asPort = 8100, asPurgeInterval = Min 5 }';

    haskell_service_var_in_shm stats 32k /tmp $hs_stats;

    haskell_run_service simpleService_reportStats $hs_reportStats 8100;

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        haskell_run updateStats !$hs_updateStats $bytes_sent;

        location / {
            echo Ok;
        }
    }

    server {
        listen       8020;
        server_name  stat;

        location / {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://127.0.0.1:8100/get/stats;
        }
    }
}
```

The aggregate service *stats* must be referred from the Nginx configuration
file with prefix __*simpleService_aggregate&#95;*__. Its configuration is typed,
the type is *AggregateServerConf*. Though its only constructor
*AggregateServerConf* is not exported from this module, the service is still
configurable from an Nginx configuration. Here, the aggregate service listens
on TCP port *8100*, and its *purge interval* is 5 minutes. Notice that an
aggregate service must be *shared* (here, variable *hs_stats* is declared as
shared with Nginx directive *haskell_service_var_in_shm*), otherwise it won't
even start because the internal HTTP servers on each worker process won't be
able to bind to the same TCP port. Inside the upper *server* clause, handler
*updateStats* runs on every client request. This handler always returns an
empty string in variable *hs_updateStats* because it is only needed for the side
effect of updating the *stats*. However, as soon as Nginx variable handlers are
*lazy*, evaluation of *hs_updateStats* must be forced somehow. To achieve this,
we used the *strict annotation* (the *bang* symbol) in directive *haskell_run*
that enforces strict evaluation in a late request processing phase, when the
value of variable *bytes_sent* has been already calculated.

Data collected by the aggregate service can be obtained in a request to the
virtual server listening on TCP port *8020*. It simply proxies requests to
the internal aggregate server with URL */get/__stats__* where __*stats*__
corresponds to the *name* of the aggregate service.

###### A simple test

As far as *reportStats* is a deferred service, we won't get useful data in 5
seconds after Nginx start.

```ShellSession
$ curl 'http://127.0.0.1:8020/' | jq
[
  "1970-01-01T00:00:00Z",
  {}
]
```

However, later we should get some useful data.

```ShellSession
$ curl 'http://127.0.0.1:8020/' | jq
[
  "2019-04-22T14:19:04Z",
  {
    "5910": [
      "2019-04-22T14:19:19Z",
      {
        "bytesSent": 0,
        "requests": 0,
        "meanBytesSent": 0
      }
    ],
    "5911": [
      "2019-04-22T14:19:14Z",
      {
        "bytesSent": 0,
        "requests": 0,
        "meanBytesSent": 0
      }
    ]
  }
]
```

Here we have collected stats from the two Nginx worker processes with *PIDs*
*5910* and *5911*. The timestamps show when the stats was updated the last
time. The topmost timestamp shows the time of the latest *purge* event. The
data itself have only zeros as soon we have made no request to the main
server so far. Let's run 100 simultaneous requests and look at the stats (it
should update at worst in 5 seconds after running them).

```ShellSession
$ for i in {1..100} ; do curl 'http://127.0.0.1:8010/' & done
```

Wait 5 seconds...

```ShellSession
$ curl 'http://127.0.0.1:8020/' | jq
[
  "2019-04-22T14:29:04Z",
  {
    "5910": [
      "2019-04-22T14:31:34Z",
      {
        "bytesSent": 17751,
        "requests": 97,
        "meanBytesSent": 183
      }
    ],
    "5911": [
      "2019-04-22T14:31:31Z",
      {
        "bytesSent": 549,
        "requests": 3,
        "meanBytesSent": 183
      }
    ]
  }
]
```

#### Module *NgxExport.Tools.EDE*

This module allows for complex parsing of JSON objects with [*EDE templating
language*](http://hackage.haskell.org/package/ede/docs/Text-EDE.html). In
terms of module *NgxExport.Tools*, it exports a *single-shot* service
*compileEDETemplates* to configure a list of templates parameterized by
a simple key, and two variable handlers *renderEDETemplate* and
*renderEDETemplateFromFreeValue* for parsing JSON objects and
substitution of extracted data into provided EDE templates. The former
handler is *asynchronous* and suitable for parsing JSON objects POSTed in a
request body, while the latter is *synchronous* and can parse JSON objects
contained in Nginx variables.

##### An example

###### File *test_tools_extra_ede.hs*

```haskell
{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraEDE where

import           NgxExport
import           NgxExport.Tools.EDE ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types.URI as URI

urlDecode :: ByteString -> L.ByteString
urlDecode = L.fromStrict . URI.urlDecode False

ngxExportYY 'urlDecode
```

We are going to use *urlDecode* to decode JSON  values contained in HTTP
cookies. Notice that we are not using any Haskell declarations from module
*NgxExport.Tools.EDE* while still need to import this to access the three
handlers from the Nginx configuration. This situation is quite valid though
not usual to *ghc*, and to make it keep silence, an explicit empty import
list was added at the end of the import stanza.

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    haskell load /var/lib/nginx/test_tools_extra_ede.so;

    haskell_run_service simpleService_compileEDETemplates $hs_EDETemplates
            '("/var/lib/nginx/EDE",
              [("user",
                "{{user.id}}/{{user.ops|b64}}/{{resources.path|uenc}}")])';

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        location / {
            haskell_run_async_on_request_body renderEDETemplate $hs_user user;
            rewrite ^ /internal/user/$hs_user last;
        }

        location ~ ^/internal/user/(EDE\ ERROR:.*) {
            internal;
            echo_status 404;
            echo "Bad input: $1";
        }

        location ~ ^/internal/user/([^/]+)/([^/]+)/([^/]+)$ {
            internal;
            echo "User id: $1, options: $2, path: $3";
        }

        location ~ ^/internal/user/(.*) {
            internal;
            echo_status 404;
            echo "Unexpected input: $1";
        }

        location /cookie {
            haskell_run urlDecode $hs_cookie_user $cookie_user;
            haskell_run renderEDETemplateFromFreeValue $hs_user_from_cookie
                    user|$hs_cookie_user;
            rewrite ^ /internal/user/$hs_user_from_cookie last;
        }
    }
}
```

There is an EDE template declared by the argument of service
*simpleService_compileEDETemplates*. The template will be accessed later
in the asynchronous body handler *renderEDETemplate* with key *user*.
Path */var/lib/nginx/EDE* can be used in the templates to *include* more
rules from files located inside it, but we do not actually use this here.

The rule inside template *user* says: with given JSON object,

* print object *id* inside a top object *user*,
* print *slash*,
* print object *ops* inside the top object *user* filtered by function *b64*,
* print *slash*,
* print object *path* inside a top object *resources* filtered by function
  *uenc*.

Functions *b64* and *uenc* are *polymorphic filters* in terms of EDE language.
There are many filters shipped with EDE, but *b64* and *uenc* were defined in
this module.

* *b64* encodes a JSON object using *base64url* encoding
* *uenc* encodes a JSON object using *URL encoding* rules

So, basically, we used *renderEDETemplate* to decompose POSTed JSON objects
and then *rewrite* requests to other locations where extracted fields were
encoded inside the location's URL path. Handler
*renderEDETemplateFromFreeValue* in *location /cookie* does the same but
reads JSON objects from HTTP cookie *user*.

###### A simple test

```ShellSession
$ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"path": "/opt/users"}}' 'http://localhost:8010/'
User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
```

Let's try to send a broken (in any meaning) input value.

```ShellSession
$ curl -d '{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"p": "/opt/users"}}' 'http://localhost:8010/'
Bad input: EDE ERROR: Text.EDE.parse:1:32 error: variable resources.path doesn't exist.
```

Now we got response with HTTP status *404* and a comprehensive description of
what went wrong. To not mess rewrite logic and error responses, variable
*hs_user* can be listed inside directive *haskell_var_empty_on_error* in the
Nginx configuration.

```nginx
    haskell_var_empty_on_error $hs_user;
```

Now the variable will always be empty on errors, while the errors will still
be logged by Nginx in the error log.

Let's read user data encoded in HTTP cookie *user*.

```ShellSession
$ curl -b 'user=%7B%22user%22%3A%20%7B%22id%22%20%3A%20%22user1%22%2C%20%22ops%22%3A%20%5B%22op1%22%2C%20%22op2%22%5D%7D%2C%20%22resources%22%3A%20%7B%22path%22%3A%20%22%2Fopt%2Fusers%22%7D%7D' 'http://localhost:8010/cookie'
User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
```

#### Module *NgxExport.Tools.Prometheus*

This module is aimed to convert custom counters from
[nginx-custom-counters-module](https://github.com/lyokha/nginx-custom-counters-module)
to Prometheus metrics. For this, it exposes three exporters:
*prometheusConf* which is an *ignitionService* in terms of module
*NgxExport.Tools*, *toPrometheusMetrics* to convert *custom counters* to
Prometheus metrics, and *scale1000*: a small utility to convert small
floating point numbers to integers by multiplying them by *1000* (this fits
well for dealing with request durations, for instance).

The module makes use of a few custom data types which are not exported while
still needed when writing Nginx configurations. In the following example they
are used in configurations of *simpleService_prometheusConf* and
*toPrometheusMetrics*.

##### An example

###### File *test_tools_extra_prometheus.hs*

```haskell
module TestToolsExtraPrometheus where

import NgxExport.Tools.Prometheus ()
```

The file does not contain any significant declarations as we are going to use
only the exporters.

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    map $status $inc_cnt_4xx {
        default                0;
        '~^4(?:\d){2}'         1;
    }

    map $status $inc_cnt_5xx {
        default                0;
        '~^5(?:\d){2}'         1;
    }

    map_to_range_index $hs_request_time $request_time_bucket
        0.005
        0.01
        0.05
        0.1
        0.5
        1.0
        5.0
        10.0
        30.0
        60.0;

    map_to_range_index $hs_bytes_sent $bytes_sent_bucket
        0
        10
        100
        1000
        10000;

    haskell load /var/lib/nginx/test_tools_extra_prometheus.so;

    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_4xx", "Number of responses with 4xx status")
                    ,("cnt_5xx", "Number of responses with 5xx status")
                    ,("cnt_stub_status_active", "Active requests")
                    ,("cnt_uptime", "Nginx master uptime")
                    ,("cnt_uptime_reload", "Nginx master uptime after reload")
                    ,("hst_request_time", "Request duration")
                    ]
                , pcGauges = ["cnt_stub_status_active"]
                , pcScale1000 = ["hst_request_time_sum"]
                }';

    haskell_var_empty_on_error $hs_prom_metrics;

    counters_survive_reload on;

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        counter $cnt_4xx inc $inc_cnt_4xx;
        counter $cnt_5xx inc $inc_cnt_5xx;

        # cache $request_time and $bytes_sent
        haskell_run ! $hs_request_time $request_time;
        haskell_run ! $hs_bytes_sent $bytes_sent;

        histogram $hst_request_time 11 $request_time_bucket;
        haskell_run scale1000 $hs_request_time_scaled $hs_request_time;
        counter $hst_request_time_sum inc $hs_request_time_scaled;

        histogram $hst_bytes_sent 6 $bytes_sent_bucket;
        counter $hst_bytes_sent_sum inc $hs_bytes_sent;

        location / {
            echo_sleep 0.5;
            echo Ok;
        }

        location /1 {
            echo_sleep 1.0;
            echo Ok;
        }

        location /404 {
            return 404;
        }
    }

    server {
        listen       8020;
        server_name  stats;

        location / {
            haskell_run toPrometheusMetrics $hs_prom_metrics
                    '["main"
                     ,$cnt_collection
                     ,$cnt_histograms
                     ,{"cnt_stub_status_active": $cnt_stub_status_active
                      ,"cnt_uptime": $cnt_uptime
                      ,"cnt_uptime_reload": $cnt_uptime_reload
                      }
                     ]';

            if ($hs_prom_metrics = '') {
                return 503;
            }

            echo -n $hs_prom_metrics;
        }

        location /counters {
            echo $cnt_collection;
        }

        location /histograms {
            echo $cnt_histograms;
        }

        location /uptime {
            echo "Uptime (after reload): $cnt_uptime ($cnt_uptime_reload)";
        }
    }
}
```

Type *PrometheusConf* contains fields *pcMetrics*, *pcGauges*, and
*pcScale1000*. Field *pcMetrics* is a map from metrics names to help
messages: this can be used to bind small descriptions to the metrics as
*nginx-custom-counters-module* does not provide such functionality. Setting
descriptions to counters is optional. Field *pcGauges* lists counters that
must be regarded as gauges: the number of currently active requests is
obviously a gauge. Field *pcScale1000* contains a list of counters that were
scaled with *scale1000* and must be converted back.

Handler *toPrometheusMetrics* expects 4 fields: the name of the
*counter set identifier* &mdash; in our example there is only one counter
set *main*, predefined variables *cnt_collection* and *cnt_histograms* from
*nginx-custom-counters-module*, and a list of additional counters &mdash; in
our example there are three additional counters *cnt_stub_status_active*,
*cnt_uptime*, and *cnt_uptime_reload* which are also defined in
*nginx-custom-counters-module*.

To fulfill histogram description in Prometheus, the *sum* value must be
provided. Histogram sums are not supported in *nginx-custom-counters-module*,
and therefore they must be declared in separate counters. In this example
there are two histograms collecting request durations and the number of sent
bytes, and accordingly, there are two sum counters: *hst_request_time_sum*
and *hst_bytes_sent_sum*. As request durations may last milliseconds while
being shown in seconds, they must be scaled with *scale1000*.

To further ensure histogram validity, it is important to have the last bucket
in a histogram labeled as *"+Inf"*. This is achieved automatically when
the number of range boundaries in directive *map_to_range_index* is less by
one than the number in the corresponding histogram declaration: in this
example, the map for *request_time_bucket* has 10 range boundaries while
histogram *hst_request_time* has 11 buckets, the map for *bytes_sent_bucket*
has 5 range boundaries while histogram *hst_bytes_sent* has 6 buckets.

###### A simple test

Let's look at the metrics right after starting Nginx.

```ShellSession
$ curl -s 'http://localhost:8020/'
# HELP cnt_4xx Number of responses with 4xx status
# TYPE cnt_4xx counter
cnt_4xx 0.0
# HELP cnt_5xx Number of responses with 5xx status
# TYPE cnt_5xx counter
cnt_5xx 0.0
# HELP cnt_stub_status_active Active requests
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 1.0
# HELP cnt_uptime Nginx master uptime
# TYPE cnt_uptime counter
cnt_uptime 8.0
# HELP cnt_uptime_reload Nginx master uptime after reload
# TYPE cnt_uptime_reload counter
cnt_uptime_reload 8.0
# HELP hst_bytes_sent 
# TYPE hst_bytes_sent histogram
hst_bytes_sent_bucket{le="0"} 0
hst_bytes_sent_bucket{le="10"} 0
hst_bytes_sent_bucket{le="100"} 0
hst_bytes_sent_bucket{le="1000"} 0
hst_bytes_sent_bucket{le="10000"} 0
hst_bytes_sent_bucket{le="+Inf"} 0
hst_bytes_sent_count 0
hst_bytes_sent_sum 0.0
# HELP hst_bytes_sent_err 
# TYPE hst_bytes_sent_err counter
hst_bytes_sent_err 0.0
# HELP hst_request_time Request duration
# TYPE hst_request_time histogram
hst_request_time_bucket{le="0.005"} 0
hst_request_time_bucket{le="0.01"} 0
hst_request_time_bucket{le="0.05"} 0
hst_request_time_bucket{le="0.1"} 0
hst_request_time_bucket{le="0.5"} 0
hst_request_time_bucket{le="1.0"} 0
hst_request_time_bucket{le="5.0"} 0
hst_request_time_bucket{le="10.0"} 0
hst_request_time_bucket{le="30.0"} 0
hst_request_time_bucket{le="60.0"} 0
hst_request_time_bucket{le="+Inf"} 0
hst_request_time_count 0
hst_request_time_sum 0.0
# HELP hst_request_time_err 
# TYPE hst_request_time_err counter
hst_request_time_err 0.0
```

Run some requests and look at the metrics again.

```ShellSession
$ for i in {1..20} ; do curl -D- 'http://localhost:8010/' & done
  ...
$ for i in {1..30} ; do curl -D- 'http://localhost:8010/1' & done
  ...
$ curl 'http://127.0.0.1:8010/404'
  ...
```

```ShellSession
$ curl -s 'http://localhost:8020/'
# HELP cnt_4xx Number of responses with 4xx status
# TYPE cnt_4xx counter
cnt_4xx 1.0
# HELP cnt_5xx Number of responses with 5xx status
# TYPE cnt_5xx counter
cnt_5xx 0.0
# HELP cnt_stub_status_active Active requests
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 1.0
# HELP cnt_uptime Nginx master uptime
# TYPE cnt_uptime counter
cnt_uptime 371.0
# HELP cnt_uptime_reload Nginx master uptime after reload
# TYPE cnt_uptime_reload counter
cnt_uptime_reload 371.0
# HELP hst_bytes_sent 
# TYPE hst_bytes_sent histogram
hst_bytes_sent_bucket{le="0"} 0
hst_bytes_sent_bucket{le="10"} 0
hst_bytes_sent_bucket{le="100"} 0
hst_bytes_sent_bucket{le="1000"} 51
hst_bytes_sent_bucket{le="10000"} 51
hst_bytes_sent_bucket{le="+Inf"} 51
hst_bytes_sent_count 51
hst_bytes_sent_sum 9458.0
# HELP hst_bytes_sent_err 
# TYPE hst_bytes_sent_err counter
hst_bytes_sent_err 0.0
# HELP hst_request_time Request duration
# TYPE hst_request_time histogram
hst_request_time_bucket{le="0.005"} 1
hst_request_time_bucket{le="0.01"} 1
hst_request_time_bucket{le="0.05"} 1
hst_request_time_bucket{le="0.1"} 1
hst_request_time_bucket{le="0.5"} 13
hst_request_time_bucket{le="1.0"} 44
hst_request_time_bucket{le="5.0"} 51
hst_request_time_bucket{le="10.0"} 51
hst_request_time_bucket{le="30.0"} 51
hst_request_time_bucket{le="60.0"} 51
hst_request_time_bucket{le="+Inf"} 51
hst_request_time_count 51
hst_request_time_sum 40.006
# HELP hst_request_time_err 
# TYPE hst_request_time_err counter
hst_request_time_err 0.0
```

---

Module *NgxExport.Tools.Prometheus* has limited support for extracting data from
lists of values. Normally, variables from Nginx upstream module such as
*upstream_status*, *upstream_response_time* and others contain lists of values
separated by commas and semicolons. With handler *statusLayout*, numbers of
*2xx*, *3xx*, *4xx* and *5xx* responses from backends can be collected in a
comma-separated list. Handlers *cumulativeValue* and *cumulativeFPValue* can be
used to count cumulative integer and floating point numbers from lists of
values.

Let's add checking upstream statuses and cumulative response times from all
servers in an upstream into the original file *nginx.conf* from the previous
example.

###### File *nginx.conf*: checking upstream statuses and response times

```nginx
    upstream backends {
        server 127.0.0.1:8030 max_fails=0;
        server 127.0.0.1:8040 max_fails=0;
    }
```

```nginx
    server {
        listen       8030;
        server_name  backend1;

        location / {
            echo_sleep 0.5;
            echo_status 404;
            echo "Backend1 Ok";
        }
    }

    server {
        listen       8040;
        server_name  backend2;

        location / {
            echo_status 504;
            echo "Backend2 Ok";
        }
    }
```

Here we added upstream *backends* with two virtual servers that will play
the role of backends. One of them will wait for half a second and return
HTTP status *404*, while the other will return HTTP status *504* immediately.
Both servers are tagged with *max_fails=0* to prevent blacklisting them.

We also have to add counters and mappings.

```nginx
    map $hs_upstream_status $inc_cnt_u_4xx {
        default                               0;
        '~^(?:(?:\d+),){2}(?P<m_status>\d+)'  $m_status;
    }

    map $hs_upstream_status $inc_cnt_u_5xx {
        default                               0;
        '~^(?:(?:\d+),){3}(?P<m_status>\d+)'  $m_status;
    }

    map_to_range_index $hs_u_response_time $u_response_time_bucket
        0.005
        0.01
        0.05
        0.1
        0.5
        1.0
        5.0
        10.0
        30.0
        60.0;
```

```nginx
        haskell_run statusLayout $hs_upstream_status $upstream_status;
        counter $cnt_u_4xx inc $inc_cnt_u_4xx;
        counter $cnt_u_5xx inc $inc_cnt_u_5xx;

        haskell_run ! $hs_u_response_times $upstream_response_time;
        haskell_run cumulativeFPValue $hs_u_response_time $hs_u_response_times;

        histogram $hst_u_response_time 11 $u_response_time_bucket;
        haskell_run scale1000 $hs_u_response_time_scaled $hs_u_response_time;
        counter $hst_u_response_time_sum inc $hs_u_response_time_scaled;
```

So many new variables require a bigger hash table to store them.

```nginx
    variables_hash_max_size 4096;
```

And finally, we have to update counters declarations in
*simpleService_prometheusConf* and add  location */backends* in the main
server.

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_4xx", "Number of responses with 4xx status")
                    ,("cnt_5xx", "Number of responses with 5xx status")
                    ,("cnt_u_4xx"
                     ,"Number of responses from upstreams with 4xx status")
                    ,("cnt_u_5xx"
                     ,"Number of responses from upstreams with 5xx status")
                    ,("cnt_stub_status_active", "Active requests")
                    ,("cnt_uptime", "Nginx master uptime")
                    ,("cnt_uptime_reload", "Nginx master uptime after reload")
                    ,("hst_request_time", "Request duration")
                    ,("hst_u_response_time"
                     ,"Response time from all servers in a single upstream")
                    ]
                , pcGauges = ["cnt_stub_status_active"]
                , pcScale1000 = ["hst_request_time_sum"
                                ,"hst_u_response_time_sum"
                                ]
                }';
```

```nginx
        location /backends {
            error_page 404 @status404;
            proxy_intercept_errors on;
            proxy_pass http://backends;
        }

        location @status404 {
            echo_sleep 0.2;
            echo "Caught 404";
        }
```

We are going to additionally increase response time by *0.2* seconds when a
backend server responds with HTTP status *404*.

Let's restart Nginx and run a simple test.

```ShellSession
$ for i in {1..20} ; do curl -D- 'http://localhost:8010/backends' & done
  ...
```

```ShellSession
$ curl -s 'http://127.0.0.1:8020/metrics'
# HELP cnt_4xx Number of responses with 4xx status
# TYPE cnt_4xx counter
cnt_4xx 11.0
# HELP cnt_5xx Number of responses with 5xx status
# TYPE cnt_5xx counter
cnt_5xx 9.0
# HELP cnt_stub_status_active Active requests
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 1.0
# HELP cnt_u_4xx Number of responses from upstreams with 4xx status
# TYPE cnt_u_4xx counter
cnt_u_4xx 11.0
# HELP cnt_u_5xx Number of responses from upstreams with 5xx status
# TYPE cnt_u_5xx counter
cnt_u_5xx 9.0
# HELP cnt_uptime Nginx master uptime
# TYPE cnt_uptime counter
cnt_uptime 63.0
# HELP cnt_uptime_reload Nginx master uptime after reload
# TYPE cnt_uptime_reload counter
cnt_uptime_reload 63.0
# HELP hst_bytes_sent
# TYPE hst_bytes_sent histogram
hst_bytes_sent_bucket{le="0"} 0
hst_bytes_sent_bucket{le="10"} 0
hst_bytes_sent_bucket{le="100"} 0
hst_bytes_sent_bucket{le="1000"} 20
hst_bytes_sent_bucket{le="10000"} 20
hst_bytes_sent_bucket{le="+Inf"} 20
hst_bytes_sent_count 20
hst_bytes_sent_sum 4032.0
# HELP hst_bytes_sent_err
# TYPE hst_bytes_sent_err counter
hst_bytes_sent_err 0.0
# HELP hst_request_time Request duration
# TYPE hst_request_time histogram
hst_request_time_bucket{le="0.005"} 9
hst_request_time_bucket{le="0.01"} 9
hst_request_time_bucket{le="0.05"} 9
hst_request_time_bucket{le="0.1"} 9
hst_request_time_bucket{le="0.5"} 9
hst_request_time_bucket{le="1.0"} 20
hst_request_time_bucket{le="5.0"} 20
hst_request_time_bucket{le="10.0"} 20
hst_request_time_bucket{le="30.0"} 20
hst_request_time_bucket{le="60.0"} 20
hst_request_time_bucket{le="+Inf"} 20
hst_request_time_count 20
hst_request_time_sum 7.721
# HELP hst_request_time_err
# TYPE hst_request_time_err counter
hst_request_time_err 0.0
# HELP hst_u_response_time Response time from all servers in a single upstream
# TYPE hst_u_response_time histogram
hst_u_response_time_bucket{le="0.005"} 9
hst_u_response_time_bucket{le="0.01"} 9
hst_u_response_time_bucket{le="0.05"} 9
hst_u_response_time_bucket{le="0.1"} 9
hst_u_response_time_bucket{le="0.5"} 13
hst_u_response_time_bucket{le="1.0"} 20
hst_u_response_time_bucket{le="5.0"} 20
hst_u_response_time_bucket{le="10.0"} 20
hst_u_response_time_bucket{le="30.0"} 20
hst_u_response_time_bucket{le="60.0"} 20
hst_u_response_time_bucket{le="+Inf"} 20
hst_u_response_time_count 20
hst_u_response_time_sum 5.519
# HELP hst_u_response_time_err
# TYPE hst_u_response_time_err counter
hst_u_response_time_err 0.0
```

Counters look good. Numbers of visiting backend servers are almost equal (11
and 9), the sum of cumulative response times from backends is approximately 5
seconds, while the sum of all requests durations is approximately 7 seconds
which corresponds to 11 visits to location *@status404* and the sleep time
*0.2* seconds that was added there. Notice that upstream response times will
be updated on entering *any* location in the main server as the histogram and
its sum counter were declared on the server level. To update the histogram on
entering location *backends* only, it is possible to move the declarations
inside this location, however in this case there will be no updates in
location *@status404* as there is no way to declare the same histogram more
than once in a single counter set. Another solution would be putting line

```nginx
            set $u_response_time_bucket unavailable;
```

into all other locations of the main server (i.e. */*, */1*, and */404*):
in this case only counter *hst_u_response_time_err* will be updated when
entering these locations.

#### Module *NgxExport.Tools.Subrequest*

Using asynchronous variable handlers and services together with the HTTP
client from *Network.HTTP.Client* allows making HTTP subrequests easily.
This module provides such functionality by exporting asynchronous variable
handlers *makeSubrequest* and *makeSubrequestWithRead*, and functions
*makeSubrequest* and *makeSubrequestWithRead* to build custom handlers.

##### An example

###### File *test_tools_extra_subrequest.hs*

```haskell
{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraSubrequest where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Subrequest

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

makeRequest :: ByteString -> Bool -> IO L.ByteString
makeRequest = const . makeSubrequest

ngxExportSimpleService 'makeRequest $ PersistentService $ Just $ Sec 10
```

Handler *makeRequest* will be used in a *periodical* service which will
retrieve data from a specified URI every 10 seconds.

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

http {
    default_type        application/octet-stream;
    sendfile            on;

    upstream backend {
        server 127.0.0.1:8020;
    }

    haskell load /var/lib/nginx/test_tools_extra_subrequest.so;

    haskell_run_service simpleService_makeRequest $hs_service_httpbin
            '{"uri": "http://httpbin.org"}';

    haskell_var_empty_on_error $hs_subrequest;

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        location / {
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:8010/proxy",
                      "headers": [["Custom-Header", "$arg_a"]]}';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }

        location /proxy {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://backend;
        }

        location /httpbin {
            echo $hs_service_httpbin;
        }
    }

    server {
        listen       8020;
        server_name  backend;

        location / {
            set $custom_header $http_custom_header;
            echo "In backend, Custom-Header is '$custom_header'";
        }
    }
}
```

Configurations of subrequests are defined via JSON objects which contain URI
and other relevant data such as HTTP method, request body and headers. In
this configuration we are running a periodical service which gets contents of
*httpbin.org* every 10 seconds, and doing a subrequest to a virtual server
*backend* on every request to *location /*. In this subrequest, an HTTP
header *Custom-Header* is sent to the backend with value equal to the value
of argument *a* from the client request's URI.

It is worth noting that making HTTP subrequests to the own Nginx service
(e.g. via *127.0.0.1*) allows for leveraging well-known advantages of Nginx
such as load-balancing via upstreams as it is happening in this example.

###### A simple test

```ShellSession
$ curl -s 'http://localhost:8010/httpbin' | head
<!DOCTYPE html>
<html lang="en">

<head>
    <meta charset="UTF-8">
    <title>httpbin.org</title>
    <link href="https://fonts.googleapis.com/css?family=Open+Sans:400,700|Source+Code+Pro:300,600|Titillium+Web:400,600,700"
        rel="stylesheet">
    <link rel="stylesheet" type="text/css" href="/flasgger_static/swagger-ui.css">
    <link rel="icon" type="image/png" href="/static/favicon.ico" sizes="64x64 32x32 16x16" />
```

```ShellSession
$ curl 'http://localhost:8010/?a=Value'
In backend, Custom-Header is 'Value'
```

Let's do a nasty thing. By injecting a comma into the argument *a* we shall
break JSON parsing.

```ShellSession
$ curl -D- 'http://localhost:8010/?a=Value"'
HTTP/1.1 404 Not Found
Server: nginx/1.17.9
Date: Mon, 30 Mar 2020 14:42:42 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive

Failed to perform subrequest
```

---

Handlers *makeSubrequest* and *makeSubrequestWithRead* return response body
of subrequests skipping the response status and headers. To retrieve full
data from a response, use another pair of asynchronous variable handlers and
functions: *makeSubrequestFull* and *makeSubrequestFullWithRead*,
and *makeSubrequestFull* and *makeSubrequestFullWithRead* respectively.

Unlike the simple body handlers, there is no sense of using the corresponding
variables directly as they are binary encoded values. Instead, the response
status, headers and the body must be extracted using handlers
*extractStatusFromFullResponse*, *extractHeaderFromFullResponse*,
and *extractBodyFromFullResponse* which are based on functions of the
same name.

Let's extend our example with these handlers.

File *test_tools_extra_subrequest.hs* does not have any changes as we are
going to use exported handlers only.

###### File *nginx.conf*: new location */full* in server *main*

```nginx
        location /full {
            haskell_run_async makeSubrequestFull $hs_subrequest
                    '{"uri": "http://127.0.0.1:$arg_p/proxy",
                      "headers": [["Custom-Header", "$arg_a"]]}';

            haskell_run extractStatusFromFullResponse $hs_subrequest_status
                    $hs_subrequest;

            haskell_run extractHeaderFromFullResponse $hs_subrequest_header
                    subrequest-header|$hs_subrequest;

            haskell_run extractBodyFromFullResponse $hs_subrequest_body
                    $hs_subrequest;

            if ($hs_subrequest_status = 400) {
                echo_status 400;
                echo "Bad request";
                break;
            }

            if ($hs_subrequest_status = 500) {
                echo_status 500;
                echo "Internal server error while making subrequest";
                break;
            }

            if ($hs_subrequest_status = 502) {
                echo_status 502;
                echo "Backend unavailable";
                break;
            }

            if ($hs_subrequest_status != 200) {
                echo_status 404;
                echo "Subrequest status: $hs_subrequest_status";
                break;
            }

            echo    "Subrequest status: $hs_subrequest_status";
            echo    "Subrequest-Header: $hs_subrequest_header";
            echo -n "Subrequest body: $hs_subrequest_body";
        }
```

Now we can recognize HTTP response statuses of subrequests and handle them
differently. We also can read a response header *Subrequest-Header*.

###### File *nginx.conf*: new response header *Subrequest-Header* in *location /* of server *backend*

```nginx
            add_header Subrequest-Header "This is response from subrequest";
```

###### A simple test

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value"'
HTTP/1.1 400 Bad Request
Server: nginx/1.17.9
Date: Sat, 04 Apr 2020 12:44:36 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive

Bad request
```

Good. Now we see that adding a comma into a JSON field is a bad request.

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value'
HTTP/1.1 500 Internal Server Error
Server: nginx/1.17.9
Date: Sat, 04 Apr 2020 12:47:11 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive

Internal server error while making subrequest
```

This is also good. Now we are going to define port of the backend server via
argument *arg_p*. Skipping this makes URI look unparsable
(*http://127.0.0.1:/*) which leads to the error.

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value&p=8020'
HTTP/1.1 200 OK
Server: nginx/1.17.9
Date: Sat, 04 Apr 2020 12:52:03 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive

Subrequest status: 200
Subrequest-Header: This is response from subrequest
Subrequest body: In backend, Custom-Header is 'Value'
```

Finally, we are getting a good response with all the response data decoded
correctly.

Let's try another port.

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value&p=8021'
HTTP/1.1 502 Bad Gateway
Server: nginx/1.17.9
Date: Sat, 04 Apr 2020 12:56:02 GMT
Content-Type: application/octet-stream
Transfer-Encoding: chunked
Connection: keep-alive

Backend unavailable
```

Good. There is no server listening on port 8021.

#### Building and installation

##### Configure and build

```ShellSession
$ cabal configure
$ cabal build
```

##### Install

```ShellSession
$ cabal install
```

or globally, being a superuser

```ShellSession
# cabal install --global
```

The module is also available on
[*hackage.haskell.org*](http://hackage.haskell.org/package/ngx-export-tools-extra),
so you can simply install it from there with

```ShellSession
$ cabal install ngx-export-tools-extra
```

or

```ShellSession
# cabal install ngx-export-tools-extra --global
```

##### Building custom libraries

See details in [test/Aggregate/README.md](test/Aggregate/README.md),
[test/EDE/README.md](test/EDE/README.md),
[test/Prometheus/README.md](test/Prometheus/README.md), and
[test/Subrequest/README.md](test/Subrequest/README.md).

