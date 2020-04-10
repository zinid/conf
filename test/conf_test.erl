%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%%-------------------------------------------------------------------
-module(conf_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    ?assertEqual(ok, conf:start()).

load_test() ->
    Expect = expect(),
    ?assertEqual(ok, conf:load_file(test_file())),
    ?assertEqual(Expect,
                 lists:keysort(1, [{App, lists:keysort(1, application:get_all_env(App))} || App <- apps()])).

stop_test() ->
    ?assertEqual(ok, conf:stop()).

%%%===================================================================
%%% Internal functions
%%%===================================================================
test_file() ->
    filename:join(test_dir(), "test.yml").

test_dir() ->
    filename:join(["$PWD", "test"]).

apps() ->
    [stdlib, kernel, ssl, mnesia, crypto,
     sasl, runtime_tools, odbc, os_mon].

expect() ->
    [{crypto,[{fips_mode,false},{rand_cache_size,1024}]},
     {kernel,[{boot_server_slaves,[{127,0,0,1}]},
              {dist_auto_connect,never},
              {global_groups,[{group1,normal,[a@localhost,b@localhost]}]},
              {inet_dist_listen_max,65535},
              {inet_dist_listen_min,1024},
              {inet_dist_use_interface,{0,0,0,0,0,0,0,0}},
              {inetrc,"/etc/hosts"},
              {logger,[{handler,default,logger_std_h,
                        #{config => #{type => standard_io},
                          formatter =>
                              {logger_formatter,#{legacy_header => true,single_line => false}}}}]},
              {logger_level,debug},
              {logger_sasl_compatible,false},
              {net_setuptime,60},
              {net_ticktime,30},
              {shell_history,enabled},
              {shell_history_drop,["q().","halt()."]},
              {shell_history_file_bytes,1024000},
              {shell_history_path,"/tmp/erlang/history"},
              {shutdown_func,{erlang,halt}},
              {shutdown_timeout,500},
              {start_boot_server,false},
              {start_disk_log,false},
              {start_dist_ac,true},
              {start_distribution,true},
              {start_pg2,false},
              {start_timer,false},
              {sync_nodes_mandatory,[a@localhost,b@localhost]},
              {sync_nodes_optional,[c@localhost]},
              {sync_nodes_timeout,700}]},
     {mnesia,[{access_module,mnesia},
              {allow_index_on_key,false},
              {auto_repair,true},
              {backup_module,mnesia_backup},
              {core_dir,false},
              {dc_dump_limit,4},
              {debug,none},
              {dir,"/tmp/mnesia"},
              {dump_disc_copies_at_startup,true},
              {dump_log_load_regulation,false},
              {dump_log_time_threshold,180000},
              {dump_log_update_in_place,true},
              {dump_log_write_threshold,1000},
              {event_module,mnesia_event},
              {extra_db_nodes,[a@localhost,b@localhost]},
              {fallback_error_function,{erlang,halt}},
              {fold_chunk_size,100},
              {ignore_fallback_at_startup,false},
              {max_wait_for_decision,infinity},
              {no_table_loaders,2},
              {pid_sort_order,standard},
              {schema,[]},
              {schema_location,opt_disc},
              {send_compressed,0}]},
     {odbc,[{port_timeout,5000}]},
     {os_mon,[{disk_almost_full_threshold,0.7},
              {disk_space_check_interval,30},
              {disksup_posix_only,true},
              {memory_check_interval,2},
              {memsup_helper_timeout,30},
              {memsup_system_only,false},
              {os_sup_enable,true},
              {os_sup_errortag,std_error},
              {os_sup_mfa,{os_sup,error_report,[std_error]}},
              {os_sup_own,"/etc"},
              {os_sup_syslogconf,"/etc/hosts"},
              {process_memory_high_watermark,0.8},
              {start_cpu_sup,false},
              {start_disksup,true},
              {start_memsup,false},
              {start_os_sup,false},
              {system_memory_high_watermark,0.9}]},
     {runtime_tools,[{ttb_autostart_module,ttb_autostart}]},
     {sasl,[{errlog_type,progress},
            {error_logger_mf_dir,false},
            {error_logger_mf_maxbytes,102400},
            {error_logger_mf_maxfiles,5},
            {sasl_error_logger,{file,"/tmp/sasl.log",
                                [write,sync,
                                 {read_ahead,4096},
                                 {delayed_write,500,10000},
                                 {encoding,utf8}]}},
            {utc_log,true}]},
     {ssl,[{dtls_protocol_version,['dtlsv1.2']},
           {internal_active_n,1},
           {protocol_version,['tlsv1.2']},
           {session_cache_client_max,1000},
           {session_cache_server_max,1000},
           {session_cb,conf},
           {session_cb_init_args,[time," [",level,"] ",pid," ",
                                  {logger_formatter,[[logger_formatter,title],":","\n"],[]},
                                  msg,"\n"]},
           {session_lifetime,3600},
           {ssl_pem_cache_clean,120000}]},
     {stdlib,[{restricted_shell,shell},
              {shell_catch_exception,true},
              {shell_esc,abort},
              {shell_history_length,100},
              {shell_prompt_func,default},
              {shell_saved_results,100},
              {shell_strings,true},
              {utc_log,true}]}].
