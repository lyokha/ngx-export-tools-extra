[![Hackage](https://img.shields.io/hackage/v/ngx-export-tools-extra.svg?label=hackage%20%7C%20ngx-export-tools-extra)](https://hackage.haskell.org/package/ngx-export-tools-extra)

This package contains a collection of Haskell modules with more extra tools for
[*Nginx Haskell module*](http://github.com/lyokha/nginx-haskell-module).

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

###### File *test_tools_extra.hs*

```haskell
{-# LANGUAGE TemplateHaskell, DeriveGeneric, TypeApplications #-}
{-# LANGUAGE OverloadedStrings, BangPatterns #-}

module TestToolsExtra where

import           NgxExport
import           NgxExport.Tools
import           NgxExport.Tools.Aggregate

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as C8L
import           Data.Aeson
import           Data.Maybe
import           Data.IORef
import           Control.Monad
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
NgxExport.ngxExportIOYY 'updateStats

reportStats :: ByteString -> Bool -> IO C8L.ByteString
reportStats = deferredService $ \conf -> do
    let port = readFromByteString @Int conf
    when (isJust port) $ do
        s <- readIORef stats
        reportAggregate (fromJust port) (Just s) "stats"
    return ""
ngxExportSimpleService 'reportStats $ PersistentService $ Just $ Sec 5

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

    log_format combined1 '$remote_addr - $remote_user [$time_local] '
                         '"$request" $status $body_bytes_sent '
                         '"$http_referer" "$http_user_agent"'
                         '$hs_updateStats';

    haskell load /var/lib/nginx/test_tools_extra.so;

    haskell_run_service simpleService_aggregate_stats $hs_stats
            'AggregateServerConf { asPort = 8100, asPurgeInterval = Min 5 }';

    haskell_service_var_in_shm stats 32k /tmp $hs_stats;

    haskell_run_service simpleService_reportStats $hs_reportStats 8100;

    server {
        listen       8010;
        server_name  main;
        error_log    /tmp/nginx-test-haskell-error.log;
        access_log   /tmp/nginx-test-haskell-access.log combined1;

        haskell_run updateStats $hs_updateStats $bytes_sent;

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
*updateStats* runs on every client request. However, as soon as Nginx
variable handlers are *lazy*, evaluation of *hs_updateStats* must be forced
somewhere: the log phase is a good choice for this (Nginx internal variable
*bytes_sent* has already been evaluated at this point). That's why
*hs_updateStats* (which is always empty, but has valuable side effects) is
put inside of the *log_format combined1* without any risk of affecting the
actual formatting.

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

