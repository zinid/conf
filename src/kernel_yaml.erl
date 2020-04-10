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
-module(kernel_yaml).

-behaviour(conf).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [enum/1, ip/0, int/2, file/0, timeout/1, timeout/2,
               bool/0, ipv4/0, atom/0, map/2, options/2, list/2,
               string/0, non_empty/1, and_then/2, directory/1]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{dist_auto_connect => enum([never, once]),
        global_groups => global_groups_validator(),
        inet_dist_use_interface => ip(),
        inet_dist_listen_min => int(0, 65535),
        inet_dist_listen_max => int(0, 65535),
        inet_parse_error_log => enum([silent]),
        inetrc => conf_misc:to_string(file()),
        net_setuptime => conf_misc:to_seconds(timeout(second)),
        net_ticktime => conf_misc:to_seconds(timeout(second)),
        shutdown_timeout => timeout(millisecond, infinity),
        sync_nodes_mandatory => list(atom(), [unique]),
        sync_nodes_optional => list(atom(), [unique]),
        sync_nodes_timeout => timeout(millisecond, infinity),
        start_distribution => bool(),
        start_dist_ac => bool(),
        start_boot_server => bool(),
        boot_server_slaves => list(ipv4(), [unique]),
        start_disk_log => bool(),
        start_pg2 => bool(),
        start_timer => bool(),
        shell_history => enum([enabled, disabled]),
        shell_history_drop => list(string(), [unique]),
        shell_history_file_bytes => int(51200, infinity),
        shell_history_path => conf_misc:to_string(directory(write)),
        shutdown_func => conf_misc:modfun_validator(1),
        logger_level => enum([all, emergency, alert, critical, error,
                              warning, notice, info, debug, none]),
        logger_sasl_compatible => bool()},
      [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec global_groups_validator() -> yval:validator().
global_groups_validator() ->
    and_then(
      map(atom(),
          options(
            #{nodes => non_empty(list(atom(), [unique])),
              publish_type => enum([normal, hidden])},
            [{required, [nodes]}, {return, map}])),
      fun(L) ->
              lists:map(
                fun({Group, #{publish_type := Type, nodes := Nodes}}) ->
                        {Group, Type, Nodes};
                   ({Group, #{nodes := Nodes}}) ->
                        {Group, Nodes}
                end, L)
      end).
