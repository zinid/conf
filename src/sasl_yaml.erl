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
-module(sasl_yaml).

-behaviour(conf).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [bool/0, enum/1, directory/1, options/2, int/2,
               pos_int/0, file/1, and_then/2, either/2]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{sasl_error_logger => sasl_error_logger_validator(),
        error_logger_mf_dir => either(false, conf_misc:to_string(directory(write))),
        error_logger_mf_maxbytes => pos_int(),
        error_logger_mf_maxfiles => int(1, 255),
        errlog_type => enum([error, progress, all]),
        utc_log => bool()},
      [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec sasl_error_logger_validator() -> yval:validator().
sasl_error_logger_validator() ->
    either(
      enum([false, tty]),
      and_then(
        options(
          #{file => file(write),
            modes => conf_misc:file_modes_validator()},
          [unique, {required, [file]}, {return, map}]),
        fun(#{file := File, modes := Modes}) ->
                {file, binary_to_list(File), Modes};
           (#{file := File}) ->
                {file, binary_to_list(File)}
        end)).
