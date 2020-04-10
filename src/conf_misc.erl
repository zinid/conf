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
-module(conf_misc).

%% API
-export([modfun_validator/1]).
-export([mfa_validator/0]).
-export([file_modes_validator/0]).
-export([to_string/1]).
-export([to_seconds/1]).
-export([to_minutes/1]).
%% Imported validators
-import(yval, [bool/0, enum/1, options/2, atom/0, and_then/2, either/2,
               pos_int/0, timeout/1, beam/1, list/1, term/0, any/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec to_seconds(yval:validator()) -> yval:validator(pos_integer()).
to_seconds(F) ->
    and_then(F, fun(T) -> T div 1000 end).

-spec to_minutes(yval:validator()) -> yval:validator(pos_integer()).
to_minutes(F) ->
    and_then(F, fun(T) -> T div 60000 end).

-spec to_string(yval:validator()) -> yval:validator(string()).
to_string(F) ->
    and_then(F, fun binary_to_list/1).

-spec modfun_validator(non_neg_integer()) -> yval:validator({module(), atom()}).
modfun_validator(Arity) ->
    and_then(
      options(
        #{module => atom(),
          function => atom()},
        [unique, {required, [module, function]}, {return, map}]),
      fun(#{module := Mod, function := Fun}) ->
              _ = (beam([{Fun, Arity}]))(Mod),
              {Mod, Fun}
      end).

-spec mfa_validator() -> yval:validator({module(), atom(), list()}).
mfa_validator() ->
    and_then(
      options(
        #{module => atom(),
          function => atom(),
          args => and_then(term(), list(any()))},
        [unique, {required, [module, function]}, {return, map}]),
      fun(#{module := Mod, function := Fun} = M) ->
              Args = maps:get(args, M, []),
              Arity = length(Args) + 1,
              _ = (beam([{Fun, Arity}]))(Mod),
              {Mod, Fun, Args}
      end).

-spec file_modes_validator() -> yval:validator([file:mode()]).
file_modes_validator() ->
    and_then(
      options(
        #{read => bool(),
          write => bool(),
          append => bool(),
          exclusive => bool(),
          raw => bool(),
          binary => bool(),
          compressed => bool(),
          ram => bool(),
          sync => bool(),
          read_ahead => either(bool(), pos_int()),
          delayed_write =>
              either(
                bool(),
                options(
                  #{size => pos_int(),
                    delay => timeout(millisecond)},
                  [unique, {required, [size, delay]}, {return, map}])),
          encoding =>
              enum([latin1, unicode, utf8,
                    utf16, 'utf16-big', 'utf16-little',
                    utf32, 'utf32-big', 'utf32-little'])},
        [unique]),
      fun(Modes) ->
              lists:filtermap(
                fun({_, false}) -> false;
                   ({Opt, true}) when Opt == read; Opt == write;
                                      Opt == append; Opt == exclusive;
                                      Opt == raw; Opt == binary;
                                      Opt == compressed; Opt == ram;
                                      Opt == sync; Opt == read_ahead;
                                      Opt == delayed_write ->
                        {true, Opt};
                   ({read_ahead, _}) ->
                        true;
                   ({delayed_write, #{size := Size, delay := Delay}}) ->
                        {true, {delayed_write, Size, Delay}};
                   ({encoding, 'utf16-big'}) ->
                        {true, {encoding, {utf16, big}}};
                   ({encoding, 'utf16-little'}) ->
                        {true, {encoding, {utf16, little}}};
                   ({encoding, 'utf32-big'}) ->
                        {true, {encoding, {utf32, big}}};
                   ({encoding, 'utf32-little'}) ->
                        {true, {encoding, {utf32, little}}};
                   ({encoding, _}) ->
                        true
                end, Modes)
      end).

%%%===================================================================
%%% Internal functions
%%%===================================================================
