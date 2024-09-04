# vi:filetype=

use Test::Nginx::Socket;

repeat_each(1);
plan tests => repeat_each() * (2 * blocks());

no_shuffle();
no_root_location();
run_tests();

__DATA__

=== TEST 1: warm
--- http_config
    variables_hash_max_size 4096;

    upstream backends {
        server 127.0.0.1:8030 max_fails=0;
        server 127.0.0.1:8040 max_fails=0;
        #server 127.0.0.1:8040;
    }

    #proxy_next_upstream error timeout http_504;

    map $status $inc_cnt_4xx {
        default         0;
        '~^4(?:\d){2}'  1;
    }

    map $status $inc_cnt_5xx {
        default         0;
        '~^5(?:\d){2}'  1;
    }

    map $hs_upstream_status $inc_cnt_u_4xx {
        default                               0;
        '~^(?:(?:\d+),){2}(?P<m_status>\d+)'  $m_status;
    }

    map $hs_upstream_status $inc_cnt_u_5xx {
        default                               0;
        '~^(?:(?:\d+),){3}(?P<m_status>\d+)'  $m_status;
    }

    map_to_range_index $hs_request_time $request_time_bucket
        0.005
        0.01
        0.05
        0.1
        0.5
        1.0
        5.0
        10.0
        30.0
        60.0;

    map_to_range_index $hs_bytes_sent $bytes_sent_bucket
        0
        10
        100
        1000
        10000;

    map_to_range_index $hs_u_response_time $u_response_time_bucket
        0.005
        0.01
        0.05
        0.1
        0.5
        1.0
        5.0
        10.0
        30.0
        60.0;

    haskell load $TEST_NGINX_TELIB_DIR/test_tools_extra_prometheus.so;

    haskell_run_service simpleService_prometheusConf $hs_prometheus_conf
            'PrometheusConf
                { pcMetrics = fromList
                    [("cnt_4xx", "Number of responses with 4xx status")
                    ,("cnt_5xx", "Number of responses with 5xx status")
                    ,("cnt_u_4xx"
                     ,"Number of responses from upstreams with 4xx status")
                    ,("cnt_u_5xx"
                     ,"Number of responses from upstreams with 5xx status")
                    ,("cnt_stub_status_active", "Active requests")
                    ,("hst_request_time", "Request duration")
                    ,("hst_u_response_time"
                     ,"Response time from all servers in a single upstream")
                    ]
                , pcGauges = fromList
                    ["cnt_stub_status_active"]
                , pcScale1000 = fromList
                    ["hst_request_time_sum"
                    ,"hst_u_response_time_sum"
                    ]
                }';

    haskell_var_empty_on_error $hs_prom_metrics;

    counters_survive_reload on;

    server {
        listen       8020;
        server_name  stats;

        location / {
            haskell_run toPrometheusMetrics $hs_prom_metrics
                    '["main"
                     ,$cnt_collection
                     ,$cnt_histograms
                     ,{"cnt_stub_status_active": $cnt_stub_status_active
                      }
                     ]';

            if ($hs_prom_metrics = '') {
                return 503;
            }

            default_type "text/plain; version=0.0.4; charset=utf-8";

            echo -n $hs_prom_metrics;
        }

        location /ch {
            haskell_async_content prometheusMetrics
                    '["main"
                     ,$cnt_collection
                     ,$cnt_histograms
                     ,{"cnt_stub_status_active": $cnt_stub_status_active
                      }
                     ]';
        }

        location /counters {
            default_type application/json;
            echo $cnt_collection;
        }

        location /histograms {
            default_type application/json;
            echo $cnt_histograms;
        }

        location /uptime {
            echo "Uptime (after reload): $cnt_uptime ($cnt_uptime_reload)";
        }
    }

    server {
        listen       8030;
        server_name  backend1;

        location / {
            echo_sleep 0.5;
            echo_status 404;
            echo "Backend1 Ok";
        }
    }

    server {
        listen       8040;
        server_name  backend2;

        location / {
            echo_status 504;
            echo "Backend2 Ok";
        }
    }
--- config
        counter_set_id  main;

        counter $cnt_4xx inc $inc_cnt_4xx;
        counter $cnt_5xx inc $inc_cnt_5xx;

        haskell_run statusLayout $hs_upstream_status $upstream_status;
        counter $cnt_u_4xx inc $inc_cnt_u_4xx;
        counter $cnt_u_5xx inc $inc_cnt_u_5xx;

        # cache $request_time and $bytes_sent
        haskell_run ! $hs_request_time $request_time;
        haskell_run ! $hs_bytes_sent $bytes_sent;

        histogram $hst_request_time 11 $request_time_bucket;
        haskell_run scale1000 $hs_request_time_scaled $hs_request_time;
        counter $hst_request_time_sum inc $hs_request_time_scaled;

        histogram $hst_bytes_sent 6 $bytes_sent_bucket;
        counter $hst_bytes_sent_sum inc $hs_bytes_sent;

        # cache $upstream_response_time
        haskell_run ! $hs_u_response_times $upstream_response_time;

        histogram $hst_u_response_time 11 $u_response_time_bucket;
        histogram $hst_u_response_time undo;
        haskell_run cumulativeFPValue $hs_u_response_time $hs_u_response_times;
        haskell_run scale1000 $hs_u_response_time_scaled $hs_u_response_time;

        location / {
            echo_sleep 0.5;
            echo Ok;
        }

        location /1 {
            echo_sleep 1.0;
            echo Ok;
        }

        location /404 {
            return 404;
        }

        location /backends {
            histogram $hst_u_response_time reuse;
            counter $hst_u_response_time_sum inc $hs_u_response_time_scaled;
            error_page 404 @status404;
            proxy_intercept_errors on;
            proxy_pass http://backends;
        }

        location @status404 {
            histogram $hst_u_response_time reuse;
            counter $hst_u_response_time_sum inc $hs_u_response_time_scaled;
            echo_sleep 0.2;
            echo "Caught 404";
        }

        location /8020 {
            proxy_pass http://127.0.0.1:8020/;
        }

        location /warm {
            histogram $hst_bytes_sent undo;
            counter $hst_bytes_sent_sum undo;
            histogram $hst_request_time undo;
            echo Ok;
        }
--- request
    GET /warm
--- wait: 2
--- response_body
Ok
--- error_code: 200

=== TEST 2: req
--- request
    GET /8020
--- response_body
# HELP cnt_4xx Number of responses with 4xx status
# TYPE cnt_4xx counter
cnt_4xx 0.0
# HELP cnt_5xx Number of responses with 5xx status
# TYPE cnt_5xx counter
cnt_5xx 0.0
# HELP cnt_stub_status_active Active requests
# TYPE cnt_stub_status_active gauge
cnt_stub_status_active 2.0
# HELP cnt_u_4xx Number of responses from upstreams with 4xx status
# TYPE cnt_u_4xx counter
cnt_u_4xx 0.0
# HELP cnt_u_5xx Number of responses from upstreams with 5xx status
# TYPE cnt_u_5xx counter
cnt_u_5xx 0.0
# HELP hst_bytes_sent 
# TYPE hst_bytes_sent histogram
hst_bytes_sent_bucket{le="0"} 0
hst_bytes_sent_bucket{le="10"} 0
hst_bytes_sent_bucket{le="100"} 0
hst_bytes_sent_bucket{le="1000"} 0
hst_bytes_sent_bucket{le="10000"} 0
hst_bytes_sent_bucket{le="+Inf"} 0
hst_bytes_sent_count 0
hst_bytes_sent_sum 0.0
# HELP hst_bytes_sent_err 
# TYPE hst_bytes_sent_err counter
hst_bytes_sent_err 0.0
# HELP hst_request_time Request duration
# TYPE hst_request_time histogram
hst_request_time_bucket{le="0.005"} 0
hst_request_time_bucket{le="0.01"} 0
hst_request_time_bucket{le="0.05"} 0
hst_request_time_bucket{le="0.1"} 0
hst_request_time_bucket{le="0.5"} 0
hst_request_time_bucket{le="1.0"} 0
hst_request_time_bucket{le="5.0"} 0
hst_request_time_bucket{le="10.0"} 0
hst_request_time_bucket{le="30.0"} 0
hst_request_time_bucket{le="60.0"} 0
hst_request_time_bucket{le="+Inf"} 0
hst_request_time_count 0
hst_request_time_sum 0.0
# HELP hst_request_time_err 
# TYPE hst_request_time_err counter
hst_request_time_err 0.0
# HELP hst_u_response_time Response time from all servers in a single upstream
# TYPE hst_u_response_time histogram
hst_u_response_time_bucket{le="0.005"} 0
hst_u_response_time_bucket{le="0.01"} 0
hst_u_response_time_bucket{le="0.05"} 0
hst_u_response_time_bucket{le="0.1"} 0
hst_u_response_time_bucket{le="0.5"} 0
hst_u_response_time_bucket{le="1.0"} 0
hst_u_response_time_bucket{le="5.0"} 0
hst_u_response_time_bucket{le="10.0"} 0
hst_u_response_time_bucket{le="30.0"} 0
hst_u_response_time_bucket{le="60.0"} 0
hst_u_response_time_bucket{le="+Inf"} 0
hst_u_response_time_count 0
hst_u_response_time_sum 0.0
# HELP hst_u_response_time_err 
# TYPE hst_u_response_time_err counter
hst_u_response_time_err 0.0
--- error_code: 200

