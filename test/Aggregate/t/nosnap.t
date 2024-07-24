# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: req
--- http_config
    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_aggregate.so;

    haskell_run_service simpleService_reportStats $hs_reportStats 8100;

    haskell_var_empty_on_error $hs_stats;

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
--- config
        haskell_run updateStats !$hs_updateStats $bytes_sent;

        location / {
            echo Ok;
        }

        location /8020 {
            proxy_pass http://127.0.0.1:8020/;
        }
--- request
    GET /
--- response_body
Ok
--- error_code: 200

=== TEST 2: check_no_data
--- init: sleep 1
--- request
    GET /8020
--- wait: 5
--- response_body chomp
["1858-11-17T00:00:00Z",{}]
--- error_code: 200

=== TEST 3: check_2_reqs
--- request
    GET /8020
--- response_body_like: ^\["[^"]+",\{"\d+":\["[^"]+",(\{"bytesSent":(?!0,)\d+\,"meanBytesSent":(?!0,)\d+,"requests":2})\]\}\]$
--- error_code: 200

