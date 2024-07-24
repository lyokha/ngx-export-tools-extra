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
    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_ede.so;

    haskell_run_service simpleService_compileEDETemplates $hs_EDETemplates
            '("/var/lib/nginx/EDE",
              [("user",
                "{{user.id}}/{{user.ops|b64}}/{{resources.path|uenc}}")])';
--- config
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
--- init: sleep 1
--- request
    POST /
{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"path": "/opt/users"}}
--- response_body
User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
--- error_code: 200

=== TEST 2: bad_req
--- request
    POST /
{"user": {"id" : "user1", "ops": ["op1", "op2"]}, "resources": {"p": "/opt/users"}}
--- response_body_like: ^Bad input: EDE ERROR: Text.EDE.parse:\d+:\d+ error: variable resources.path doesn't exist.$
--- error_code: 404

=== TEST 3: cookie_req
--- request
    GET /cookie
--- more_headers
Cookie: user=%7B%22user%22%3A%20%7B%22id%22%20%3A%20%22user1%22%2C%20%22ops%22%3A%20%5B%22op1%22%2C%20%22op2%22%5D%7D%2C%20%22resources%22%3A%20%7B%22path%22%3A%20%22%2Fopt%2Fusers%22%7D%7D
--- response_body
User id: user1, options: WyJvcDEiLCJvcDIiXQ==, path: %2Fopt%2Fusers
--- error_code: 200

