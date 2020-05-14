[![Hackage](https://img.shields.io/hackage/v/ngx-export-tools-extra.svg?label=hackage%20%7C%20ngx-export-tools-extra)](https://hackage.haskell.org/package/ngx-export-tools-extra)

This package contains a collection of Haskell modules with more extra tools for
[*Nginx Haskell module*](http://github.com/lyokha/nginx-haskell-module).

#### Table of contents

- [Module NgxExport.Tools.Aggregate](#module-ngxexporttoolsaggregate)
- [Module NgxExport.Tools.EDE](#module-ngxexporttoolsede)
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
[test/EDE/README.md](test/EDE/README.md), and
[test/Subrequest/README.md](test/Subrequest/README.md).

