### 1.2.11

- Module *NgxExport.Tools.Resolve*.
  + A smarter implementation of the internal hashable data *HashUQuery*.
- Module *NgxExport.Tools.PCRE*.
  + Replace *rareService* mode declaration in service *declareRegexes* by
    *restartPromptly* declaration from *ngx-export-tools &ge; 1.2.6*.

### 1.2.10

- Module *NgxExport.Tools.Resolve*.
  + Parameterize collected server data by query data to avoid getting wrong
    values when multiple instances of the resolve service are running.
  + Use exporters from *ngx-export-tools &ge; 1.2.5* which do not share
    configurations between multiple instances of the resolve service.
  + Append port numbers to values of collected host names as Nginx directive
    *server* accepts such a notation.
- Module *NgxExport.Tools.PCRE*.
  + Replace *SingleShotService* mode declaration in service *declareRegexes* by
    *rareService* declaration from *ngx-export-tools &ge; 1.2.5*.

### 1.2.9.1

- Module *NgxExport.Tools.Resolve*.
  + By mistake, servers with less weights had higher priorities in *A* queries.
  + If the number of upstreams in the priority list is less than the number of
    variations of priorities collected in *SRV* queries or weights set in
    *QueryA* data, then remaining servers with the lowest priorities or weights
    won't be used in the upstreams.
  + With the *PriorityList* policy, weights of servers in the collected data are
    deliberately not specified in *A* queries. In *SRV* queries, conversely,
    weights are not specified with the *SinglePriority* policy as it is not
    clear how to choose them correctly from the two parameters *priority* and
    *weight* of the *SRV* record.

### 1.2.9

- Module *NgxExport.Tools.Resolve*.
  + Correctly print IP addresses in the server data collected in *A* queries.
  + Allow setting weights to names in *QueryA* data.
  + Allow priority policies in *QueryA* data.
  + If the number of upstreams in the priority list is greater than the number
    of variations of priorities collected in *SRV* queries or weights set in
    *QueryA* data, then remaining upstreams will contain the same servers with
    the lowest priority or weight rather than not being specified at all in the
    collected data.
  + With the *PriorityList* policy, weights of servers in the collected data are
    deliberately not specified.
  + Names in the *QueryA* name list may contain suffix *:port* where *port* is
    a port number, this suffix is ignored at resolving step in *collectA* and
    only appended to values of the collected server addresses.

### 1.2.8.1

- Module *NgxExport.Tools.PCRE*.
  + Use *voidServer* from *ngx-export-tools &ge; 1.2.3*.
- Module *NgxExport.Tools.ServiceHookAdaptor*.
  + The test example was improved and extended.

### 1.2.8

- Module *NgxExport.Tools.Subrequest*.
  + Allow setting custom HTTP managers in subrequest configurations.
  + In subrequest configurations, field *useUDS* / *srUseUDS* is no longer
    accepted, use *"manager": "uds"* / *srManager = UDS* instead.

### 1.2.7

- Support package *base64* *1.0*.

### 1.2.6

- Refactored to avoid using *head* and *tail* as it triggers *x-partial*
  warnings in GHC *9.8.1*.
- Use declarations *voidHandler*, *voidHandler'*, and *NgxExportService* from
  package *ngx-export-tools* *1.2.2*.

### 1.2.5

- Module *NgxExport.Tools.Resolve*.
  + Function *collectSrv* now returns a *TTL* with a list of pairs
    *(Domain name, IP address)* wrapped in an *SRV* container.
  + Collection *ServerData* now contains the host name.
  + Allow multiple *upconf* endpoints, secure and insecure.
  + Removed *instance FromJSON ServerData*.
- Module *NgxExport.Tools.PCRE*.
  + Removed internal implementation of function *compile* which now requires
    *pcre-light &ge; 0.4.1.2*.

### 1.2.4

- Added support for *https* connections in module *NgxExport.Tools.Subrequest*.

### 1.2.3

- In module *NgxExport.Tools.EDE*, custom EDE filters are now passed into
  function *renderEDETemplateWith*. The collection of custom EDE filters with
  *b64* and *uenc* filters is now exported from the module by name
  *extraEDEFilters*.

### 1.2.2

- Added Cabal constraint *resolv &ge; 0.2.0.1* to fix memory leaks in module
  *NgxExport.Tools.Resolve*.

### 1.2.1

- Improvements and bug fixes in module *NgxExport.Tools.Resolve*.
  + Added ability to set response timeout in service *collectUpstreams*.
  + Use *handleAny* from *safe-exceptions* in service *collectUpstreams*.
  + Improved type declarations and documentation.

### 1.2.0

- Added module *NgxExport.Tools.Resolve* which can be used to manage service
  discovery in Nginx dynamic upstreams.

### 1.1.0

- Using *Network.HTTP.Client.BrReadWithTimeout* in module
  *NgxExport.Tools.Subrequest* to ensure that a request won't stall on a bad
  response from a buggy server.
- In module *NgxExport.Tools.Subrequest*, *srBody* was reimplemented using lazy
  bytestrings.

### 1.0

- Module *NgxExport.Tools.Aggregate* can now be built without support from the
  *Snap framework* (flag *SnapAggregateServer* replaces flag *Aggregate*). The
  module now allows building native Nginx-based aggregate services (see updated
  docs).
- In module *NgxExport.Tools.Aggregate*, getting current time with *ngxNow* was
  replaced by *getCurrentTime* because the former is not safe in the async
  context.
- Removed upper bound restriction in the Cabal constraint on the version of
  package *aeson*.
- Package stability tag was promoted to stable.

### 0.8.2.0

- In module *NgxExport.Tools.Subrequest*, bridged HTTP subrequests return status
  code of the source end of the bridge when it returns a *non-2xx* response.

### 0.8.1.0

- In module *NgxExport.Tools.Subrequest*, bridged HTTP subrequests were added.
- In module *NgxExport.Tools.Subrequest*, configured subrequest headers now get
  merged with originally set request headers.
- Added Cabal constraint *aeson &lt; 2.0.0.0* for building EDE module.

### 0.8.0.0

- Added module *NgxExport.Tools.PCRE*, this requires *nginx-haskell-module*
  *2.8.4* or newer.

### 0.7.0.0

- Added module *NgxExport.Tools.ServiceHookAdaptor* to maintain custom global
  data in all the worker processes in run-time.

### 0.6.2.0

- Added Cabal flag *Aggregate* to build module *NgxExport.Tools.Aggregate*
  conditionally.

### 0.6.1.0

- Handler *extractRequestStatusFromFullResponse* was replaced with handler
  *extractExceptionFromFullResponse*. The new handler extracts the error message
  if the subrequest was terminated by an exception.

### 0.6.0.0

- Build EDE module with *prettyprinter* automatically.

### 0.5.9.0

- Allow Unix domain sockets in configurations of internal HTTP subrequests.

### 0.5.8.0

- Using *Data.HashSet* for *pcGauges* and *pcScale1000* in Prometheus metrics.

### 0.5.7.0

- Show histogram *error* counters in parameterized Prometheus metrics.

### 0.5.6.2

- Fixed wrong partitioning of gauges and counters from *special* counters.

### 0.5.6.0

- Added labeled parameterization for all supported types of metrics by using
  annotated counters from
  [*nginx-custom-counters-module*](https://github.com/lyokha/nginx-custom-counters-module).

### 0.5.5.0

- Added content handler *prometheusMetrics*.
- Using lazy text for encoding Prometheus metrics.

### 0.5.4.0

- Added handler *extractRequestStatusFromFullResponse* to retrieve completion
  status of a subrequest.

### 0.5.3.0

- Handlers *makeSubrequestFull* and *makeSubrequestFullWithRead* no longer throw
  errors on responses with HTTP statuses other than *2xx*.
- Implemented forwarding responses downstream to the client with function
  *contentFromFullResponse* and content handler *fromFullResponse*.

### 0.5.2.0

- Added handlers to extract data from lists of values in the Prometheus module.

### 0.5.1.0

- Added gauge metrics type to Prometheus counters.

### 0.5.0.0

- Added module *NgxExport.Tools.Prometheus* to convert custom counters from
  [*nginx-custom-counters-module*](https://github.com/lyokha/nginx-custom-counters-module)
  to Prometheus metrics.

### 0.4.1.0

- Handler *renderEDETemplateFromFreeValue* was moved from the example to the
  module.

### 0.4.0.0

- Implemented adjustable response timeouts.
- Implemented subrequest handlers with ability to extract full response data.

### 0.3.2.0

- Functions and handlers *subrequest* and *subrequestWithRead* were renamed to
  *makeSubrequest* and *makeSubrequestWithRead* respectively.

### 0.3.1.0

- Added function *subrequestWithRead* and an asynchronous variable handler of
  the same name.

### 0.3.0.0

- Added module *NgxExport.Tools.Subrequest* for making HTTP (sub)requests
  easily.

### 0.2.2.0

- Added Cabal flag *EDE* to detect automatically if EDE module can be built.
- Added manual Cabal flag *ExperimentalEDE* to compile against patched *ede*
  package where package *ansi-wl-pprint* was replaced with *prettyprinter*.
- The EDE example was enhanced to show how to parse free JSON values.

### 0.2.1.0

- Improved treatment of quoted string values in *b64* and *uenc* EDE filters.
- Added function *renderEDETemplateWith* for templating from any configuration
  language which is translatable to *Aeson*'s *Value*.

### 0.2.0.0

- Added module *NgxExport.Tools.EDE* for parsing JSON values.

### 0.1.0.1

- Docs improved (using *strict variable handlers* from the latest
  *nginx-haskell-module* in the example).

### 0.1.0.0

- Initial version.

