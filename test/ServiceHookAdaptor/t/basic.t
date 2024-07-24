# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: service_not_ready
--- http_config
    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_servicehookadaptor.so;

    haskell_run_service simpleService_hookAdaptor $hs_hook_adaptor noarg;

    haskell_service_hooks_zone hooks 32k;
--- config
        location / {
            haskell_run testSecretWord $hs_secret_word $arg_s;

            if ($hs_secret_word = unset) {
                echo_status 503;
                echo "Try later! The service is not ready!";
                break;
            }

            if ($hs_secret_word = success) {
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

        location /reset_sw {
            allow 127.0.0.1;
            deny all;

            rewrite ^ /change_sw last;
        }
--- init: sleep 1
--- request
    GET /
--- response_body
Try later! The service is not ready!
--- error_code: 503

=== TEST 2: change_secret
--- request
    GET /change_sw?s=secret
--- response_body
--- error_code: 200

=== TEST 3: no_secret
--- request
    GET /
--- response_body
Hmm, you do not know a secret!
--- error_code: 404

=== TEST 4: bad_secret
--- request
    GET /?s=try1
--- response_body
Hmm, you do not know a secret!
--- error_code: 404

=== TEST 5: know_secret
--- request
    GET /?s=secret
--- response_body
Congrats! You know the secret word!
--- error_code: 200

=== TEST 6: change_secret_again
--- request
    GET /change_sw?s=secret1
--- response_body
--- error_code: 200

=== TEST 7: old_secret
--- request
    GET /?s=secret
--- response_body
Hmm, you do not know a secret!
--- error_code: 404

=== TEST 8: know_secret_again
--- request
    GET /?s=secret1
--- response_body
Congrats! You know the secret word!
--- error_code: 200

