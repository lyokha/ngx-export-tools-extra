More extra tools for Nginx Haskell module
=========================================

<!--[![Build Status](https://travis-ci.com/lyokha/ngx-export-tools-extra.svg?branch=master)](https://travis-ci.com/lyokha/ngx-export-tools-extra)-->
[![Build Status](https://github.com/lyokha/ngx-export-tools-extra/workflows/CI/badge.svg)](https://github.com/lyokha/ngx-export-tools-extra/actions?query=workflow%3ACI)
[![Hackage](https://img.shields.io/hackage/v/ngx-export-tools-extra.svg?label=hackage%20%7C%20ngx-export-tools-extra)](https://hackage.haskell.org/package/ngx-export-tools-extra)

This package contains a collection of Haskell modules with more extra tools for
[*Nginx Haskell module*](http://github.com/lyokha/nginx-haskell-module).
Detailed documentation on each module's exported functions and data can be found
at [*the Hackage page*](http://hackage.haskell.org/package/ngx-export-tools-extra).

#### Table of contents

- [Module NgxExport.Tools.Aggregate](#module-ngxexporttoolsaggregate)
- [Module NgxExport.Tools.EDE](#module-ngxexporttoolsede)
- [Module NgxExport.Tools.PCRE](#module-ngxexporttoolspcre)
- [Module NgxExport.Tools.Prometheus](#module-ngxexporttoolsprometheus)
- [Module NgxExport.Tools.ServiceHookAdaptor](#module-ngxexporttoolsservicehookadaptor)
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
$ curl -s 'http://127.0.0.1:8020/' | jq
[
  "1858-11-17T00:00:00Z",
  {}
]
```

However, later we should get some useful data.

```ShellSession
$ curl -s 'http://127.0.0.1:8020/' | jq
[
  "2021-12-08T09:56:18.118132083Z",
  {
    "21651": [
      "2021-12-08T09:56:18.12155413Z",
      {
        "meanBytesSent": 0,
        "requests": 0,
        "bytesSent": 0
      }
    ],
    "21652": [
      "2021-12-08T09:56:18.118132083Z",
      {
        "meanBytesSent": 0,
        "requests": 0,
        "bytesSent": 0
      }
    ]
  }
]
```

Here we have collected stats from the two Nginx worker processes with *PIDs*
*21651* and *21652*. The timestamps show when the stats was updated the last
time. The topmost timestamp shows the time of the latest *purge* event. The
data itself have only zeros as soon we have made no request to the main
server so far. Let's run 100 simultaneous requests and look at the stats (it
should update at worst in 5 seconds after running them).

```ShellSession
$ for i in {1..100} ; do curl 'http://127.0.0.1:8010/' & done
```

Wait 5 seconds...

```ShellSession
$ curl -s 'http://127.0.0.1:8020/' | jq
[
  "2021-12-08T09:56:18.118132083Z",
  {
    "21651": [
      "2021-12-08T09:56:48.159263993Z",
      {
        "meanBytesSent": 183,
        "requests": 84,
        "bytesSent": 15372
      }
    ],
    "21652": [
      "2021-12-08T09:56:48.136934713Z",
      {
        "meanBytesSent": 183,
        "requests": 16,
        "bytesSent": 2928
      }
    ]
  }
]
```

---

Service *simpleService_aggregate_stats* was implemented using
*Snap framework*. Basically, a native Nginx implementation is not easy
because the service must listen on a single (not duplicated) file descriptor
which is not the case when Nginx spawns more than one worker processes.
Running *simpleService_aggregate_stats* as a shared service is an elegant
solution as shared services guarantee that they occupy only one worker at a
time. However, *nginx-haskell-module* provides directive *single_listener*
which can be used to apply the required restriction in a custom Nginx virtual
server. This directive requires that the virtual server listens with option
*reuseport* and is only available on Linux with socket option
*SO_ATTACH_REUSEPORT_CBPF*.

Exporter *ngxExportAggregateService* exports additional handlers to build a
native Nginx-based aggregate service. Let's replace service
*simpleService_aggregate_stats* from the previous example with such a native
Nginx-based aggregate service using *single_listener* and listening on port
*8100*.

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

    haskell_run_service simpleService_reportStats $hs_reportStats 8100;

    haskell_var_empty_on_error $hs_stats;

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

    server {
        listen          8100 reuseport;
        server_name     stats;

        single_listener on;

        location /put/stats {
            haskell_run_async_on_request_body receiveAggregate_stats
                    $hs_stats "Min 1";

            if ($hs_stats = '') {
                return 400;
            }

            return 200;
        }

        location /get/stats {
            haskell_async_content sendAggregate_stats noarg;
        }
    }
}
```

Handler *receiveAggregate_stats* accepts a time interval corresponding to the
value of *asPurgeInterval* from service *simpleService_aggregate_stats*. If
the value is not readable (say, *noarg*) then it is defaulted to *Min 5*.

Notice that the stats server must listen on address *127.0.0.1* because
service *simpleService_reportStats* reports stats to this address.

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
and then *rewrite* requests to other locations where the URL path after
substitution of the extracted and then encoded into variable *hs_user*
fields points to. Handler *renderEDETemplateFromFreeValue* in location
*/cookie* does the same but reads JSON objects from HTTP cookie *user*.

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

#### Module *NgxExport.Tools.PCRE*

This module provides a simple handler *matchRegex* to match a value
against a PCRE regex preliminary declared and compiled in
*configuration service* *simpleService_declareRegexes* (which is an
*ignitionService* in terms of module *NgxExport.Tools*) and the corresponding
*service update hook* (in terms of module *NgxExport*) *compileRegexes*
at the start of the service.

##### An example

###### File *test_tools_extra_pcre.hs*

```haskell
module TestToolsExtraPCRE where

import NgxExport.Tools.PCRE ()
```

The file does not contain any significant declarations as we are going to use
only the exporters of the handlers.

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

    haskell load /var/lib/nginx/test_tools_extra_pcre.so;

    haskell_run_service simpleService_declareRegexes $hs_regexes
            '[("userArea", "(?:\\\\|)(\\\\d+)$", "")
             ,("keyValue", "(k\\\\w+)(\\\\|)(v\\\\w+)", "i")
             ]';

    haskell_var_empty_on_error $hs_kv;

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log;

        location / {
            haskell_run matchRegex $hs_user_area 'userArea|$arg_user';
            rewrite ^ /internal/user/area/$hs_user_area last;
        }

        location ~ ^/internal/user/area/(PCRE\ ERROR:.*) {
            internal;
            echo_status 404;
            echo "Bad input: $1";
        }

        location = /internal/user/area/ {
            internal;
            echo_status 404;
            echo "No user area attached";
        }

        location ~ ^/internal/user/area/(.+) {
            internal;
            echo "User area: $1";
        }
    }
}
```

In this example, we expect requests with argument *user* which should
supposedly be tagged with an *area* code containing digits only. The *user*
value should match against regex *userArea* declared alongside with another
regex *keyValue* (the latter has an option *i* which corresponds to
*caseless*; the regex compiler has also support for options *s* and *m* which
correspond to *dotall* and *multiline* respectively). Notice that regex
declarations require 4-fold backslashes as they are getting shrunk while
interpreted sequentially by the Nginx configuration interpreter and then by
the Haskell compiler too.

Handler *matchRegex* finds the named regex *userArea* from the beginning of
its argument: the second part of the argument is delimited by a *bar* symbol
and contains the value to match against. If the regex contains captures, then
the matched value shall correspond to the contents of the first capture (in
case of *userArea*, this is the area code), otherwise it must correspond to
the whole matched value.

###### A simple test

```ShellSession
$ curl 'http://localhost:8010/'
No user area attached
$ curl 'http://localhost:8010/?user=peter|98'
User area: 98
$ curl 'http://localhost:8010/?user=peter|98i'
No user area attached
```

---

There are handlers to make substitutions using PCRE regexes. An
*ignitionService* *simpleService_mapSubs* declares named *plain*
substitutions which are made in run-time by handlers *subRegex* and
*gsubRegex*. Functions *subRegexWith* and *gsubRegexWith* make it
possible to write custom *functional* substitutions.

Let's extend our example by adding ability to erase the captured area code.
We also going to implement a *functional* substitution to swap the keys and
the values matched in the *keyValue* regex.

###### File *test_tools_extra_pcre.hs*

```haskell
{-# LANGUAGE TemplateHaskell #-}

module TestToolsExtraPCRE where

import           NgxExport
import           NgxExport.Tools.PCRE

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

gsubSwapAround :: ByteString -> IO L.ByteString
gsubSwapAround = gsubRegexWith $ \_ (a : d : b : _) -> B.concat [b, d, a]

ngxExportIOYY 'gsubSwapAround
```

Functional substitution handler *gsubSwapAround* expects a regular expression
with at least 3 capture groups to swap the contents of the first and the
third groups around. We are going to apply this handler against regex
*keyValue*.

###### File *nginx.conf*: erase area code and swap keys and values

```nginx
    haskell_run_service simpleService_mapSubs $hs_subs
            '[("erase", "")]';

    haskell_var_empty_on_error $hs_kv;
```

```nginx
        location /erase/area {
            haskell_run subRegex $hs_user_no_area 'userArea|erase|$arg_user';
            rewrite ^ /internal/user/noarea/$hs_user_no_area last;
        }

        location ~ ^/internal/user/noarea/(PCRE\ ERROR:.*) {
            internal;
            echo_status 404;
            echo "Bad input: $1";
        }

        location ~ ^/internal/user/noarea/(.*) {
            internal;
            echo "User without area: $1";
        }

        location /swap {
            haskell_run gsubSwapAround $hs_kv 'keyValue|$arg_kv';
            echo "Swap $arg_kv = $hs_kv";
        }
```

Service *simpleService_mapSubs* declares a list of named *plain*
substitutions. In this example, it declares only one substitution *erase*
which substitutes an empty string, i.e. *erases* the matched text. Notice
that the argument of handler *subRequest* requires three parts delimited by
*bar* symbols: the named regex, the named substitution, and the value to
match against.

###### A simple test

```ShellSession
$ curl 'http://localhost:8010/erase/area?user=peter|98'
User without area: peter
$ curl 'http://localhost:8010/swap?kv=kid|v0012a
Swap kid|v0012a = v0012a|kid
```

#### Module *NgxExport.Tools.Prometheus*

This module is aimed to convert custom counters from
[nginx-custom-counters-module](https://github.com/lyokha/nginx-custom-counters-module)
to Prometheus metrics. For this, it exposes four exporters:
*prometheusConf* which is an *ignitionService* in terms of module
*NgxExport.Tools*, *toPrometheusMetrics* to convert *custom counters* to
Prometheus metrics, *prometheusMetrics* which is a content handler aiming
to return Prometheus metrics to the client, and a handy utility
*scale1000* to convert small floating point numbers to integers by
multiplying them by *1000* (which fits well for dealing with request
durations).

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
        default         0;
        '~^4(?:\d){2}'  1;
    }

    map $status $inc_cnt_5xx {
        default         0;
        '~^5(?:\d){2}'  1;
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
                , pcGauges = fromList
                    ["cnt_stub_status_active"]
                , pcScale1000 = fromList
                    ["hst_request_time_sum"]
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

            default_type "text/plain; version=0.0.4; charset=utf-8";

            echo -n $hs_prom_metrics;
        }

        location /counters {
            default_type application/json;
            echo $cnt_collection;
        }

        location /histograms {
            default_type application/json;
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

Notice that the variable handler *toPrometheusMetrics* and directive *echo*
in location */* can be replaced with a single content handler
*prometheusMetrics* like in the following block.

```nginx
        location / {
            haskell_async_content prometheusMetrics
                    '["main"
                     ,$cnt_collection
                     ,$cnt_histograms
                     ,{"cnt_stub_status_active": $cnt_stub_status_active
                      ,"cnt_uptime": $cnt_uptime
                      ,"cnt_uptime_reload": $cnt_uptime_reload
                      }
                     ]';
        }
```

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

        # cache $upstream_response_time
        haskell_run ! $hs_u_response_times $upstream_response_time;

        histogram $hst_u_response_time 11 $u_response_time_bucket;
        histogram $hst_u_response_time undo;
        haskell_run cumulativeFPValue $hs_u_response_time $hs_u_response_times;
        haskell_run scale1000 $hs_u_response_time_scaled $hs_u_response_time;
```

Notice that histogram *hst_u_response_time* was disabled on this level to
not count visiting unrelated locations (i.e. */*, */1*, and */404*): the
histogram will be re-enabled later in locations related to proxying requests.
The sum counter will also be declared inside the proxying locations and take
the value of *hs_u_response_time_scaled* as the input value.

So many new variables require a bigger hash table to store them.

```nginx
    variables_hash_max_size 4096;
```

And finally, we have to update counters declarations in
*simpleService_prometheusConf* and add location */backends* in the main
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
                , pcGauges = fromList
                    ["cnt_stub_status_active"]
                , pcScale1000 = fromList
                    ["hst_request_time_sum"
                    ,"hst_u_response_time_sum"
                    ]
                }';
```

```nginx
        location /backends {
            histogram $hst_u_response_time reuse;
            counter $hst_u_response_time_sum inc $hs_u_response_time_scaled;
            error_page 404 @status404;
            proxy_intercept_errors on;
            proxy_pass http://backends;
        }

        location @status404 {
            histogram $hst_u_response_time reuse;
            counter $hst_u_response_time_sum inc $hs_u_response_time_scaled;
            echo_sleep 0.2;
            echo "Caught 404";
        }
```

We are going to additionally increase response time by *0.2* seconds when a
backend server responds with HTTP status *404*, and this is why location
*@status404* was added.

###### A simple test

Let's restart Nginx and run a simple test.

```ShellSession
$ for i in {1..20} ; do curl -D- 'http://localhost:8010/backends' & done
  ...
```

```ShellSession
$ curl -s 'http://127.0.0.1:8020/'
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
*0.2* seconds that was added there.

---

In the previous examples we used many counters which served similar purposes.
For example, counters *cnt_4xx*, *cnt_5xx*, *cnt_u_4xx*, and *cnt_u_5xx*
counted response statuses in different conditions: particularly, the 2 former
counters counted *4xx* and *5xx* response statuses sent to clients, while the
latter 2 counters counted *4xx* and *5xx* response statuses received from the
upstream. It feels that they could be shown as a single compound counter
parameterized by the range of values and the origin. We also had two
histograms *hst_request_time* and *hst_u_response_time* which could also be
combined in a single entity parameterized by the scope (the time of the whole
request against the time spent in the upstream).

Fortunately, Prometheus provides a mechanism to make such custom
parameterizations by using *labels* in metrics. This module supports the
parameterization with labels by expecting special *annotations* attached to
the names of the counters.

Let's parameterize the status counters and the request times as it was
proposed at the beginning of this section.

###### File *nginx.conf*: changes related to counters annotations

```nginx
    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_status", "Number of responses with given status")
                    ,("cnt_stub_status_active", "Active requests")
                    ,("cnt_uptime", "Nginx master uptime")
                    ,("cnt_uptime_reload", "Nginx master uptime after reload")
                    ,("hst_request_time", "Request duration")
                    ]
                , pcGauges = fromList
                    ["cnt_stub_status_active"]
                , pcScale1000 = fromList
                    ["hst_request_time@scope=(total)_sum"
                    ,"hst_request_time@scope=(in_upstreams)_sum"
                    ]
                }';
```

```nginx
        counter $cnt_status@value=(4xx),from=(response) inc $inc_cnt_4xx;
        counter $cnt_status@value=(5xx),from=(response) inc $inc_cnt_5xx;

        haskell_run statusLayout $hs_upstream_status $upstream_status;
        counter $cnt_status@value=(4xx),from=(upstream) inc $inc_cnt_u_4xx;
        counter $cnt_status@value=(5xx),from=(upstream) inc $inc_cnt_u_5xx;

        # cache $request_time and $bytes_sent
        haskell_run ! $hs_request_time $request_time;
        haskell_run ! $hs_bytes_sent $bytes_sent;

        histogram $hst_request_time@scope=(total) 11 $request_time_bucket;
        haskell_run scale1000 $hs_request_time_scaled $hs_request_time;
        counter $hst_request_time@scope=(total)_sum inc $hs_request_time_scaled;

        histogram $hst_bytes_sent 6 $bytes_sent_bucket;
        counter $hst_bytes_sent_sum inc $hs_bytes_sent;

        # cache $upstream_response_time
        haskell_run ! $hs_u_response_times $upstream_response_time;

        histogram $hst_request_time@scope=(in_upstreams) 11
                $u_response_time_bucket;
        histogram $hst_request_time@scope=(in_upstreams) undo;
        haskell_run cumulativeFPValue $hs_u_response_time $hs_u_response_times;
        haskell_run scale1000 $hs_u_response_time_scaled $hs_u_response_time;

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

        location /backends {
            histogram $hst_request_time@scope=(in_upstreams) reuse;
            counter $hst_request_time@scope=(in_upstreams)_sum inc
                    $hs_u_response_time_scaled;
            error_page 404 @status404;
            proxy_intercept_errors on;
            proxy_pass http://backends;
        }

        location @status404 {
            histogram $hst_request_time@scope=(in_upstreams) reuse;
            counter $hst_request_time@scope=(in_upstreams)_sum inc
                    $hs_u_response_time_scaled;
            echo_sleep 0.2;
            echo "Caught 404";
        }
```

Notice that the 4 status counters were combined into a compound counter
*cnt_status* whose name was annotated by a tail starting with *@*. This
annotation gets put in the list of labels of the Prometheus metrics with
symbols *(* and *)* replaced by *"* without any further validation. The
request time histograms and the corresponding sum counters were annotated in
a similar way. Annotations in histogram sum counters must be put between the
base name of the counter and the suffix *_sum*.

###### A simple test

```ShellSession
$ curl 'http://127.0.0.1:8010/404'
  ...
$ for i in {1..20} ; do curl -D- 'http://localhost:8010/backends' & done
  ...
```

```ShellSession
$ curl -s 'http://localhost:8020/' 
# HELP cnt_status Number of responses with given status
# TYPE cnt_status counter
cnt_status{value="4xx",from="response"} 11.0
cnt_status{value="4xx",from="upstream"} 10.0
cnt_status{value="5xx",from="response"} 10.0
cnt_status{value="5xx",from="upstream"} 10.0
# HELP cnt_stub_status_active Active requests
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 1.0
# HELP cnt_uptime Nginx master uptime
# TYPE cnt_uptime counter
cnt_uptime 70.0
# HELP cnt_uptime_reload Nginx master uptime after reload
# TYPE cnt_uptime_reload counter
cnt_uptime_reload 70.0
# HELP hst_bytes_sent 
# TYPE hst_bytes_sent histogram
hst_bytes_sent_bucket{le="0"} 0
hst_bytes_sent_bucket{le="10"} 0
hst_bytes_sent_bucket{le="100"} 0
hst_bytes_sent_bucket{le="1000"} 21
hst_bytes_sent_bucket{le="10000"} 21
hst_bytes_sent_bucket{le="+Inf"} 21
hst_bytes_sent_count 21
hst_bytes_sent_sum 4348.0
# HELP hst_bytes_sent_err 
# TYPE hst_bytes_sent_err counter
hst_bytes_sent_err 0.0
# HELP hst_request_time Request duration
# TYPE hst_request_time histogram
hst_request_time_bucket{le="0.005",scope="in_upstreams"} 10
hst_request_time_bucket{le="0.01",scope="in_upstreams"} 10
hst_request_time_bucket{le="0.05",scope="in_upstreams"} 10
hst_request_time_bucket{le="0.1",scope="in_upstreams"} 10
hst_request_time_bucket{le="0.5",scope="in_upstreams"} 14
hst_request_time_bucket{le="1.0",scope="in_upstreams"} 20
hst_request_time_bucket{le="5.0",scope="in_upstreams"} 20
hst_request_time_bucket{le="10.0",scope="in_upstreams"} 20
hst_request_time_bucket{le="30.0",scope="in_upstreams"} 20
hst_request_time_bucket{le="60.0",scope="in_upstreams"} 20
hst_request_time_bucket{le="+Inf",scope="in_upstreams"} 20
hst_request_time_count{scope="in_upstreams"} 20
hst_request_time_sum{scope="in_upstreams"} 5.012
hst_request_time_bucket{le="0.005",scope="total"} 11
hst_request_time_bucket{le="0.01",scope="total"} 11
hst_request_time_bucket{le="0.05",scope="total"} 11
hst_request_time_bucket{le="0.1",scope="total"} 11
hst_request_time_bucket{le="0.5",scope="total"} 11
hst_request_time_bucket{le="1.0",scope="total"} 21
hst_request_time_bucket{le="5.0",scope="total"} 21
hst_request_time_bucket{le="10.0",scope="total"} 21
hst_request_time_bucket{le="30.0",scope="total"} 21
hst_request_time_bucket{le="60.0",scope="total"} 21
hst_request_time_bucket{le="+Inf",scope="total"} 21
hst_request_time_count{scope="total"} 21
hst_request_time_sum{scope="total"} 7.02
# HELP hst_request_time_err 
# TYPE hst_request_time_err counter
hst_request_time_err{scope="in_upstreams"} 0.0
hst_request_time_err{scope="total"} 0.0
```

#### Module *NgxExport.Tools.ServiceHookAdaptor*

This module exports a *simple service* (in terms of module *NgxExport.Tools*)
*simpleService_hookAdaptor* which sleeps forever. Its sole purpose is to
serve *service hooks* for changing global data in all the worker processes in
run-time. A single service hook adaptor can serve any number of service hooks
with any type of global data.

##### An example

###### File *test_tools_extra_servicehookadaptor.hs*

```haskell
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module TestToolsExtraServiceHookAdaptor where

import           NgxExport
import           NgxExport.Tools.ServiceHookAdaptor ()

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import           Data.IORef
import           System.IO.Unsafe

secretWord :: IORef ByteString
secretWord = unsafePerformIO $ newIORef ""
{-# NOINLINE secretWord #-}

testSecretWord :: ByteString -> IO L.ByteString
testSecretWord v = do
    s <- readIORef secretWord
    return $ if B.null s
                 then "null"
                 else if v == s
                          then "set"
                          else "unset"
ngxExportIOYY 'testSecretWord

changeSecretWord :: ByteString -> IO L.ByteString
changeSecretWord s = do
    writeIORef secretWord s
    return "The secret word was changed"
ngxExportServiceHook 'changeSecretWord
```

Here we are going to maintain a *secret word* of type *ByteString* in
run-time. When a worker process starts, the word is empty. The word can be
changed in run-time by triggering service hook *changeSecretWord*. Client
requests are managed differently depending on their knowledge of the secret
which is tested in handler *testSecretWord*.

###### File *nginx.conf*

```nginx
user                    nobody;
worker_processes        2;

events {
    worker_connections  1024;
}

error_log               /tmp/nginx-test-haskell-error.log info;

http {
    default_type        application/octet-stream;
    sendfile            on;
    error_log           /tmp/nginx-test-haskell-error.log info;
    access_log          /tmp/nginx-test-haskell-access.log;

    haskell load /var/lib/nginx/test_tools_extra_servicehookadaptor.so;

    haskell_run_service simpleService_hookAdaptor $hs_hook_adaptor '';

    haskell_service_hooks_zone hooks 32k;

    server {
        listen       8010;
        server_name  main;

        location / {
            haskell_run testSecretWord $hs_secret_word $arg_s;

            if ($hs_secret_word = null) {
                echo_status 503;
                echo "Try later! The service is not ready!";
                break;
            }

            if ($hs_secret_word = set) {
                echo_status 200;
                echo "Congrats! You know the secret word!";
                break;
            }

            echo_status 404;
            echo "Hmm, you do not know a secret!";
        }

        location /change_sw {
            allow 127.0.0.1;
            deny all;

            haskell_service_hook changeSecretWord $hs_hook_adaptor $arg_s;
        }
    }
}
```

Notice that service *simpleService_hookAdaptor* is not shared, however this
is not such important because shared services must work as well.

###### A simple test

After starting Nginx, the secret word service must be not ready.

```ShellSession
$ curl 'http://127.0.0.1:8010/'
Try later! The service is not ready!
```

Let's change the secret word,

```ShellSession
$ curl 'http://127.0.0.1:8010/change_sw?s=secret'
```

and try again.

```ShellSession
$ curl 'http://127.0.0.1:8010/'
Hmm, you do not know a secret!
$ curl 'http://127.0.0.1:8010/?s=try1'
Hmm, you do not know a secret!
$ curl 'http://127.0.0.1:8010/?s=secret'
Congrats! You know the secret word!
```

Change the secret word again.

```ShellSession
$ curl 'http://127.0.0.1:8010/change_sw?s=secret1'
$ curl 'http://127.0.0.1:8010/?s=secret'
Hmm, you do not know a secret!
$ curl 'http://127.0.0.1:8010/?s=secret1'
Congrats! You know the secret word!
```

What if a worker process quits for some reason or crashes? Let's try!

```ShellSession
# ps -ef | grep nginx | grep worker
nobody     13869   13868  0 15:43 ?        00:00:00 nginx: worker process
nobody     13870   13868  0 15:43 ?        00:00:00 nginx: worker process
# kill -QUIT 13869 13870
# ps -ef | grep nginx | grep worker
nobody     14223   13868  4 15:56 ?        00:00:00 nginx: worker process
nobody     14224   13868  4 15:56 ?        00:00:00 nginx: worker process
```

```ShellSession
$ curl 'http://127.0.0.1:8010/?s=secret1'
Congrats! You know the secret word!
```

Our secret is still intact! This is because service hooks manage new worker
processes so well as those that were running when a hook was triggered.

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
                    '{"uri": "http://127.0.0.1:8010/proxy"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     }';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }

        location ~ ^/proxy(.*) {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://backend$1;
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
*backend* on every request to location */*. In this subrequest, an HTTP
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

Making HTTP subrequests to the own Nginx service via the loopback interface
(e.g. via *127.0.0.1*) has disadvantages of being neither very fast (if
compared with various types of local data communication channels) nor very
secure. Unix domain sockets is a better alternative in this sense. This
module has support for them by providing configuration service
*simpleService_configureUDS* where path to the socket can be set, and an
extra field *srUseUDS* in data *SubrequestConf*.

To extend the previous example for using with Unix domain sockets, the
following declarations should be added.

###### File *nginx.conf*: configuring the Unix domain socket

```nginx
    haskell_run_service simpleService_configureUDS $hs_service_uds
            'UDSConf {udsPath = "/tmp/backend.sock"}';
```

*UDSConf* is an opaque type containing only one field *udsPath* with the path
to the socket.

###### File *nginx.conf*: new location */uds* in server *main*

```nginx
        location /uds {
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://backend_proxy/"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     ,"useUDS": true
                     }';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }
```

###### File *nginx.conf*: new virtual server *backend_proxy*

```nginx
    server {
        listen       unix:/tmp/backend.sock;
        server_name  backend_proxy;

        location / {
            proxy_pass http://backend;
        }
    }
```

The server listens on the Unix domain socket with the path configured in
service *simpleService_configureUDS*.

###### A simple test

```ShellSession
$ curl 'http://localhost:8010/uds?a=Value'
In backend, Custom-Header is 'Value'
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
same name. Handler *extractExceptionFromFullResponse* and the
corresponding function can be used to extract the error message if an
exception has happened while making the subrequest: the value is empty if
there was no exception.

Let's extend our example with these handlers.

File *test_tools_extra_subrequest.hs* does not have any changes as we are
going to use exported handlers only.

###### File *nginx.conf*: new location */full* in server *main*

```nginx
        location /full {
            haskell_run_async makeSubrequestFull $hs_subrequest
                    '{"uri": "http://127.0.0.1:$arg_p/proxy"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     }';

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

###### File *nginx.conf*: new response header *Subrequest-Header* in location */* of server *backend*

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

---

Data encoded in the full response can be translated to *ContentHandlerResult*
and forwarded downstream to the client in directive *haskell_content*.
Handlers *fromFullResponse* and *fromFullResponseWithException*
perform such a translation. Not all response headers are allowed being
forwarded downstream, and thus the handlers delete response headers with
names listed in set *notForwardableResponseHeaders* as well as all headers
with names starting with *X-Accel-* before sending the response to the
client. The set of not forwardable response headers can be customized in
function *contentFromFullResponse*.

Let's forward responses in location */full* when argument *proxy* in the
client request's URI is equal to *yes*.

###### File *nginx.conf*: forward responses from location */full*

```nginx
            set $proxy_with_exception $arg_proxy$arg_exc;

            if ($proxy_with_exception = yesyes) {
                haskell_content fromFullResponseWithException $hs_subrequest;
                break;
            }

            if ($arg_proxy = yes) {
                haskell_content fromFullResponse $hs_subrequest;
                break;
            }
```

###### A simple test

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value&p=8020&proxy=yes'
HTTP/1.1 200 OK
Server: nginx/1.17.9
Date: Fri, 24 Jul 2020 13:14:33 GMT
Content-Type: application/octet-stream
Content-Length: 37
Connection: keep-alive
Subrequest-Header: This is response from subrequest

In backend, Custom-Header is 'Value'
```

Now let's get an error message in the response after feeding a wrong port
value.

```ShellSession
$ curl -D- 'http://localhost:8010/full/?a=Value&p=8021&proxy=yes&exc=yes'
HTTP/1.1 502 Bad Gateway
Server: nginx/1.19.4
Date: Mon, 14 Dec 2020 08:24:22 GMT
Content-Length: 593
Connection: keep-alive

HttpExceptionRequest Request {
  host                 = "127.0.0.1"
  port                 = 8021
  secure               = False
  requestHeaders       = [("Custom-Header","Value")]
  path                 = "/proxy"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
 (ConnectionFailure Network.Socket.connect: <socket: 31>: does not exist (Connection refused))
```

---

A bridged HTTP subrequest streams the response body from the *source* end of
the *bridge* to the *sink* end. Both source and sink are subrequests
configured with the familiar type *SubrequestConf*. They comprise another
opaque type *BridgeConf*. The bridge abstraction is useful when some data is
going to be copied from some source to some destination.

A bridge can be configured using handlers *makeBridgedSubrequest*,
*makeBridgedSubrequestWithRead*, *makeBridgedSubrequestFull*, and
*makeBridgedSubrequestFullWithRead* derived from the functions with the
same names.

Let's extend our example with bridged subrequests.

###### File *test_tools_extra_subrequest.hs*: auxiliary read body handler

```haskell
reqBody :: L.ByteString -> ByteString -> IO L.ByteString
reqBody = const . return

ngxExportAsyncOnReqBody 'reqBody
```

In this example, we are going to collect the request body at the sink end
with an auxiliary handler *reqBody*.

###### File *nginx.conf*: upstream *sink*

```nginx
    upstream sink {
        server 127.0.0.1:8030;
    }
```

###### File *nginx.conf*: new location */bridge* in server *main*

```nginx
        location /bridge {
            haskell_run_async makeBridgedSubrequestFull $hs_subrequest
                    '{"source":
                        {"uri": "http://127.0.0.1:$arg_p/proxy/bridge"
                        ,"headers": [["Custom-Header", "$arg_a"]]
                        }
                     ,"sink":
                        {"uri": "http://sink_proxy/echo"
                        ,"useUDS": true
                        }
                     }';

            if ($arg_exc = yes) {
                haskell_content fromFullResponseWithException $hs_subrequest;
                break;
            }

            haskell_content fromFullResponse $hs_subrequest;
        }
```

###### File *nginx.conf*: new location */bridge* in server *backend*

```nginx
        location /bridge {
            set $custom_header $http_custom_header;
            add_header Subrequest-Header "This is response from subrequest";
            echo "The response may come in chunks!";
            echo "In backend, Custom-Header is '$custom_header'";
        }
```

###### File *nginx.conf*: new server *sink*

```nginx
    server {
        listen       unix:/tmp/backend.sock;
        server_name  sink_proxy;

        location / {
            proxy_pass http://sink;
        }
    }

    server {
        listen       8030;
        server_name  sink;

        location /echo {
            haskell_run_async_on_request_body reqBody $hs_rb noarg;
            add_header Bridge-Header
                    "This response was bridged from subrequest";
            echo "Here is the bridged response:";
            echo -n $hs_rb;
        }
    }
```

Upon receiving a request with URI */bridge* at the main server, we are going
to connect to the *source* with the same URI at the server with port equal to
argument *arg_p*, and then stream its response body to a *sink* with URI
*/echo* via proxy server *sink_proxy*. Using an internal Nginx proxy server
for the sink end of the bridge is necessary if the sink end does not
recognize chunked HTTP requests! Note also that *method* of the sink
subrequest is always *POST* independently of whether or not and how exactly
it was specified.

The source end puts into the bridge channel its response headers except those
listed in *notForwardableResponseHeaders* and those with names starting with
*X-Accel-*. The request headers listed in the sink configuration get also
sent: their values override the values of the headers of the same names sent
in the response from the source end of the bridge.

Bridged HTTP subrequests have transactional semantics: any errors occurred at
either end of a bridge make the whole subrequest fail. Responses from the
source end of a bridge with *non-2xx* status codes are regarded as a failure.

In this example, after receiving all streamed data the sink collects the
request body in variable *hs_rb* and merely sends it back as a response to
the original bridged subrequest. Then this response gets decoded with
handlers *fromFullResponse* or *fromFullResponseWithException* and finally
returned in the response to the client.

###### A simple test

```ShellSession
$ curl -D- 'http://localhost:8010/bridge?a=Value&p=8010&exc=yes'
HTTP/1.1 200 OK
Server: nginx/1.19.4
Date: Tue, 19 Oct 2021 13:12:46 GMT
Content-Type: application/octet-stream
Content-Length: 100
Connection: keep-alive
Bridge-Header: This response was bridged from subrequest

Here is the bridged response:
The response may come in chunks!
In backend, Custom-Header is 'Value'
```

A negative case.

```ShellSession
$ curl -D- 'http://localhost:8010/bridge?a=Value&p=8021&exc=yes'
HTTP/1.1 502 Bad Gateway
Server: nginx/1.19.4
Date: Tue, 19 Oct 2021 13:16:18 GMT
Content-Length: 600
Connection: keep-alive

HttpExceptionRequest Request {
  host                 = "127.0.0.1"
  port                 = 8021
  secure               = False
  requestHeaders       = [("Custom-Header","Value")]
  path                 = "/proxy/bridge"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = ResponseTimeoutDefault
  requestVersion       = HTTP/1.1
  proxySecureMode      = ProxySecureWithConnect
}
 (ConnectionFailure Network.Socket.connect: <socket: 32>: does not exist (Connection refused))
```

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

The module is also available at
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
[test/PCRE/README.md](test/PCRE/README.md),
[test/Prometheus/README.md](test/Prometheus/README.md),
[test/ServiceHookAdaptor/README.md](test/ServiceHookAdaptor/README.md), and
[test/Subrequest/README.md](test/Subrequest/README.md).

