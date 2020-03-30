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
-module(stdlib_yaml).

-behaviour(conf).

%% API
-export([options/0]).
%% Imported validators
-import(yval, [non_neg_int/0, bool/0, enum/1, beam/0, either/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec options() -> conf:validators().
options() ->
    #{utc_log => bool(),
      shell_esc => enum([icl, abort]),
      restricted_shell => beam(),
      shell_catch_exception => bool(),
      shell_history_length => non_neg_int(),
      shell_prompt_func => either(default, conf_misc:modfun_validator(1)),
      shell_saved_results => non_neg_int(),
      shell_strings => bool()}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
