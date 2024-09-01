# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks()) + 2;

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: simple
--- http_config
    upstream backend {
        server 127.0.0.1:8020;
    }

    upstream sink {
        server 127.0.0.1:8030;
    }

    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_subrequest.so;

    haskell_run_service simpleService_configureUDS $hs_service_uds
            'UDSConf {udsPath = "/tmp/backend.sock"}';

    haskell_run_service simpleService_configureUdsManager $hs_service_myuds
            '/tmp/myuds.sock';

    haskell_var_empty_on_error $hs_subrequest;

    server {
        listen       unix:/tmp/backend.sock;
        server_name  backend_proxy;

        location / {
            proxy_pass http://backend;
        }
    }

    server {
        listen       unix:/tmp/myuds.sock;
        server_name  backend_proxy_myuds;

        location / {
            proxy_pass http://backend;
        }
    }

    server {
        listen       8020;
        server_name  backend;

        location / {
            set $custom_header $http_custom_header;
            add_header Subrequest-Header "This is response from subrequest";
            echo "In backend, Custom-Header is '$custom_header'";
        }

        location /bridge {
            set $custom_header $http_custom_header;
            add_header Subrequest-Header "This is response from subrequest";
            echo "The response may come in chunks!";
            echo "In backend, Custom-Header is '$custom_header'";
        }
    }

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
--- config
        location / {
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://127.0.0.1:1984/proxy"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     }';

# the same configuration for using with makeSubrequestWithRead
#
#                    'SubrequestConf { srMethod = ""
#                                    , srUri = "http://127.0.0.1:1984/proxy"
#                                    , srBody = ""
#                                    , srHeaders = [("Custom-Header", "$arg_a")]
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = Default
#                                    }';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }

        location /uds {
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://backend_proxy/"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     ,"manager": "uds"
                     }';

# the same configuration for using with makeSubrequestWithRead
#
#                    'SubrequestConf { srMethod = ""
#                                    , srUri = "http://backend_proxy/"
#                                    , srBody = ""
#                                    , srHeaders = [("Custom-Header", "$arg_a")]
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = UDS
#                                    }';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }

        location /myuds {
            haskell_run_async makeSubrequest $hs_subrequest
                    '{"uri": "http://backend_proxy_myuds/"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     ,"manager": "myuds"
                     }';

# the same configuration for using with makeSubrequestWithRead
#
#                    'SubrequestConf { srMethod = ""
#                                    , srUri = "http://backend_proxy_myuds/"
#                                    , srBody = ""
#                                    , srHeaders = [("Custom-Header", "$arg_a")]
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = Custom "myuds"
#                                    }';

            if ($hs_subrequest = '') {
                echo_status 404;
                echo "Failed to perform subrequest";
                break;
            }

            echo -n $hs_subrequest;
        }

        location /full {
            haskell_run_async makeSubrequestFull $hs_subrequest
                    '{"uri": "http://127.0.0.1:$arg_p/proxy"
                     ,"headers": [["Custom-Header", "$arg_a"]]
                     }';

# the same configuration for using with makeSubrequestWithRead
#
#                    'SubrequestConf { srMethod = ""
#                                    , srUri = "http://127.0.0.1:$arg_p/proxy"
#                                    , srBody = ""
#                                    , srHeaders = [("Custom-Header", "$arg_a")]
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = Default
#                                    }';

            set $proxy_with_exception $arg_proxy$arg_exc;

            if ($proxy_with_exception = yesyes) {
                haskell_content fromFullResponseWithException $hs_subrequest;
                break;
            }

            if ($arg_proxy = yes) {
                haskell_content fromFullResponse $hs_subrequest;
                break;
            }

            haskell_run extractStatusFromFullResponse $hs_subrequest_status
                    $hs_subrequest;

            haskell_run extractHeaderFromFullResponse $hs_subrequest_header
                    subrequest-header|$hs_subrequest;

            haskell_run extractBodyFromFullResponse $hs_subrequest_body
                    $hs_subrequest;

#            haskell_run extractExceptionFromFullResponse $hs_subrequest_error
#                    $hs_subrequest;
#
#            if ($hs_subrequest_error) {
#                echo_status 502;
#                echo "Subrequest exception: $hs_subrequest_error";
#                break;
#            }

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

        location /bridge {
            haskell_run_async makeBridgedSubrequestFull $hs_subrequest
                    '{"source":
                        {"uri": "http://127.0.0.1:$arg_p/proxy/bridge"
                        ,"headers": [["Custom-Header", "$arg_a"]]
                        }
                     ,"sink":
                        {"uri": "http://sink_proxy/echo"
                        ,"manager": "uds"
                        }
                     }';

# the same configuration for using with makeBridgedSubrequestWithRead
#
#                    'BridgeConf { bridgeSource = SubrequestConf
#                                    { srMethod = ""
#                                    , srUri =
#                                          "http://127.0.0.1:$arg_p/proxy/bridge"
#                                    , srBody = ""
#                                    , srHeaders = [("Custom-Header", "$arg_a")]
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = Default
#                                    }
#                                , bridgeSink = SubrequestConf
#                                    { srMethod = ""
#                                    , srUri = "http://sink_proxy/echo"
#                                    , srBody = ""
#                                    , srHeaders = []
#                                    , srResponseTimeout =
#                                          ResponseTimeout (Sec 30)
#                                    , srManager = UDS
#                                    }
#                                }';

            if ($arg_exc = yes) {
                haskell_content fromFullResponseWithException $hs_subrequest;
                break;
            }

            haskell_content fromFullResponse $hs_subrequest;
        }

        location ~ ^/proxy(.*) {
            allow 127.0.0.1;
            deny all;
            proxy_pass http://backend$1;
        }
--- request
    GET /?a=Value
--- response_body
In backend, Custom-Header is 'Value'
--- error_code: 200

=== TEST 2: simple_bad_request
--- request
    GET /?a=Value"
--- response_body
Failed to perform subrequest
--- error_code: 404

=== TEST 3: uds
--- request
    GET /uds?a=Value
--- response_body
In backend, Custom-Header is 'Value'
--- error_code: 200

=== TEST 4: full_no_port
--- request
    GET /full?a=Value
--- response_body
Internal server error while making subrequest
--- error_code: 500

=== TEST 5: full_good_port
--- request
    GET /full?a=Value&p=8020
--- response_body
Subrequest status: 200
Subrequest-Header: This is response from subrequest
Subrequest body: In backend, Custom-Header is 'Value'
--- error_code: 200

=== TEST 6: full_bad_port
--- request
    GET /full?a=Value&p=8021
--- response_body
Backend unavailable
--- error_code: 502

=== TEST 7: forward_full_response
--- request
    GET /full?a=Value&p=8020&proxy=yes
--- response_headers
Subrequest-Header: This is response from subrequest
--- response_body
In backend, Custom-Header is 'Value'
--- error_code: 200

=== TEST 8: bridged_subrequest
--- request
    GET /bridge?a=Value&p=1984&exc=yes
--- response_headers
Bridge-Header: This response was bridged from subrequest
--- response_body
Here is the bridged response:
The response may come in chunks!
In backend, Custom-Header is 'Value'
--- error_code: 200

