# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: req1
--- http_config
    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_pcre.so;

    haskell_run_service simpleService_declareRegexes $hs_regexes
            '[("userArea", "(?:\\\\|)(\\\\d+)$", "")
             ,("keyValue", "(k\\\\w+)(\\\\|)(v\\\\w+)", "i")
             ]';

    haskell_service_update_hook compileRegexes $hs_regexes;

    haskell_run_service simpleService_mapSubs $hs_subs
            '[("erase", "")]';

    haskell_var_empty_on_error $hs_kv;
--- config
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
--- init: sleep 1
--- request
    GET /
--- response_body
No user area attached
--- error_code: 404

=== TEST 2: req2
--- request
    GET /?user=peter|98
--- response_body
User area: 98
--- error_code: 200

=== TEST 3: req3
--- request
    GET /?user=peter|98i
--- response_body
No user area attached
--- error_code: 404

=== TEST 4: erase
--- request
    GET /erase/area?user=peter|98
--- response_body
User without area: peter
--- error_code: 200

=== TEST 5: swap
--- request
    GET /swap?kv=kid|v0012a
--- response_body
Swap kid|v0012a = v0012a|kid
--- error_code: 200

