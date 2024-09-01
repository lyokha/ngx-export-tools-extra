# vi:filetype=

use Test::Nginx::Socket;

repeat_each(2);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: upstreams
--- http_config
    upstream utest {
        zone utest 64k;
        upconf_round_robin;
        server localhost:9000;
    }

   upstream utest1 {
       zone utest1 64k;
       upconf_round_robin;
       server localhost:9000;
   }

#    upstrand utest {
#        upstream utest;
#        upstream utest1;
#        order per_request;
#        next_upstream_statuses error timeout 5xx;
#        next_upstream_timeout 60s;
#    }

    upstream utest_srv {
        zone utest_srv 64k;
        upconf_round_robin;
        server localhost:9000;
    }

    upstream utest1_srv {
        zone utest1_srv 64k;
        upconf_round_robin;
        server localhost:9000;
    }

    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_resolve.so;

    haskell_run_service simpleService_collectUpstreams $hs_upstreams
        'Conf { upstreams =
                    [UData { uQuery =
                                 QueryA
                                     (WeightedList
                                         [(Name "www.resolve.test:8020", 2)
                                         ,(Name "www.resolve1.test:8030", 1)
                                         ]
                                     ) (PriorityList ["utest", "utest1"])
                           , uMaxFails = 1
                           , uFailTimeout = 10
                           }
                    ]
              , maxWait = Sec 300
              , waitOnException = Sec 2
              , responseTimeout = Unset
              }';

    haskell_run_service simpleService_collectUpstreams $hs_upstreams_srv
        'Conf { upstreams =
                    [UData { uQuery =
                                 QuerySRV
                                     (Name "_http._tcp.resolve.test")
                                         (SinglePriority "utest_srv")
                           , uMaxFails = 1
                           , uFailTimeout = 10
                           }
                    ]
              , maxWait = Sec 300
              , waitOnException = Sec 2
              , responseTimeout = Unset
              }';

#     haskell_run_service simpleService_collectUpstreams $hs_upstreams_srv
#         'Conf { upstreams =
#                     [UData { uQuery =
#                                  QuerySRV
#                                      (Name "_http._tcp.pkg.freebsd.org")
#                                          (PriorityList
#                                              ["utest_srv", "utest1_srv"]
#                                          )
#                            , uMaxFails = 1
#                            , uFailTimeout = 10
#                            }
#                     ]
#               , maxWait = Sec 300
#               , waitOnException = Sec 2
#               , responseTimeout = Unset
#               }';

    haskell_service_var_ignore_empty $hs_upstreams $hs_upstreams_srv;
    haskell_service_var_in_shm upstreams 64k /tmp
        $hs_upstreams $hs_upstreams_srv;

    haskell_service_var_update_callback simpleService_signalUpconf
        $hs_upstreams '["http://127.0.0.1:8010/upconf"]';

    haskell_service_var_update_callback simpleService_signalUpconf
        $hs_upstreams_srv '["http://127.0.0.1:8010/upconf_srv"]';

    server {
        listen          127.0.0.1:9000;
        server_name     backend9000;

        location / {
            echo_status 503;
            echo "Not configured";
        }
    }

    server {
        listen          127.0.0.2:8020;
        server_name     www.resolve.test;

        location / {
            echo "In www.resolve.test";
        }
    }

    server {
        listen          127.0.0.3:8030;
        server_name     www.resolve1.test;

        location / {
            echo "In www.resolve1.test";
        }
    }
--- config
        location /upconf {
            upconf $hs_upstreams;

            allow 127.0.0.1;
            deny  all;
        }

        location /upconf_srv {
            upconf $hs_upstreams_srv;

            allow 127.0.0.1;
            deny  all;
        }

        location /upstreams {
            default_type application/json;
            echo $hs_upstreams;

            allow 127.0.0.1;
            deny  all;
        }

        location /upstreams_srv {
            default_type application/json;
            echo $hs_upstreams_srv;

            allow 127.0.0.1;
            deny  all;
        }

        location /utest {
            proxy_pass http://utest;
        }

        location /utest1 {
            proxy_pass http://utest1;
        }

        location /utest_srv {
            proxy_pass http://utest_srv;
        }

        location /utest1_srv {
            proxy_pass http://utest1_srv;
        }

#         location /upstrand {
#             proxy_pass http://$upstrand_utest;
#         }
--- init: sleep 1
--- request
    GET /upstreams
--- response_body
{"utest":[{"addr":"127.0.0.2:8020","fail_timeout":10,"host":"www.resolve.test:8020","max_fails":1}],"utest1":[{"addr":"127.0.0.3:8030","fail_timeout":10,"host":"www.resolve1.test:8030","max_fails":1}]}
--- error_code: 200

=== TEST 2: upstreams_srv
--- request
    GET /upstreams_srv
--- response_body
{"utest_srv":[{"addr":"127.0.0.2:8020","fail_timeout":10,"host":"www.resolve.test","max_fails":1}]}
--- error_code: 200

=== TEST 3: utest
--- request
    GET /utest
--- response_body
In www.resolve.test
--- error_code: 200

=== TEST 4: utest1
--- request
    GET /utest1
--- response_body
In www.resolve1.test
--- error_code: 200

=== TEST 5: utest_srv
--- request
    GET /utest_srv
--- response_body
In www.resolve.test
--- error_code: 200

=== TEST 6: utest1_srv
--- request
    GET /utest1_srv
--- response_body
Not configured
--- error_code: 503

