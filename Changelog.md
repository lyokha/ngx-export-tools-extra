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

