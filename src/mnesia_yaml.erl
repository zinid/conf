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
-module(mnesia_yaml).

-behaviour(conf).

%% API
-export([options/0]).
%% Imported validators
-import(yval, [directory/1, beam/0, bool/0, enum/1, pos_number/0,
               pos_int/0, pos_int/1, timeout/1, timeout/2, list/1,
               list/2, atom/0, int/2, any/0, either/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec options() -> conf:validators().
options() ->
    #{access_module => beam(),
      allow_index_on_key => bool(),
      auto_repair => bool(),
      backup_module => beam(),
      debug => enum([none, verbose, debug, trace, false, true]),
      core_dir => either(false, conf_misc:to_string(directory(write))),
      dc_dump_limit => pos_number(),
      dir => conf_misc:to_string(directory(write)),
      dump_disc_copies_at_startup => bool(),
      dump_log_load_regulation => bool(),
      dump_log_update_in_place => bool(),
      dump_log_write_threshold => pos_int(),
      dump_log_time_threshold => timeout(millisecond),
      event_module => beam(),
      extra_db_nodes => list(atom(), [unique]),
      fallback_error_function => conf_misc:modfun_validator(1),
      fold_chunk_size => pos_int(infinity),
      ignore_fallback_at_startup => bool(),
      max_wait_for_decision => timeout(millisecond, infinity),
      no_table_loaders => pos_int(),
      pid_sort_order => enum([r9b_plain, standard]),
      send_compressed => int(0, 9),
      schema => list(any()),
      schema_location => enum([disc, ram, opt_disc])}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
