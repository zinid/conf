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
-module(os_mon_yaml).

-behaviour(conf).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [bool/0, timeout/1, atom/0, directory/0, file/0,
               percent/0, options/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{start_cpu_sup => bool(),
        start_disksup => bool(),
        start_memsup => bool(),
        start_os_sup => bool(),
        memsup_system_only => bool(),
        memory_check_interval => conf_misc:to_minutes(timeout(minute)),
        system_memory_high_watermark => percent(),
        process_memory_high_watermark => percent(),
        memsup_helper_timeout => conf_misc:to_seconds(timeout(second)),
        disk_space_check_interval => conf_misc:to_minutes(timeout(minute)),
        disk_almost_full_threshold => percent(),
        disksup_posix_only => bool(),
        os_sup_mfa => conf_misc:mfa_validator(),
        os_sup_enable => bool(),
        os_sup_errortag => atom(),
        os_sup_own => conf_misc:to_string(directory()),
        os_sup_syslogconf => conf_misc:to_string(file())},
      [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
