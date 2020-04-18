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
-module(conf_yaml_backend).

%% API
-export([read_file/1]).
-export([validate/1]).
-export([format_error/1]).
-export_type([error_reason/0]).

-type error_reason() :: {unsupported_application, atom()} |
                        {invalid_yaml_config, yval:error_reason(), yval:ctx()} |
                        {bad_yaml, term()}.
-type distance_cache() :: #{{string(), string()} => non_neg_integer()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec read_file(file:filename_all()) -> {ok, term()} | {error, {bad_yaml, term()}}.
read_file(Path) ->
    case fast_yaml:decode_from_file(Path) of
        {ok, [Y]} ->
            {ok, Y};
        {ok, []} ->
            {ok, []};
        {error, Reason} ->
            {error, {bad_yaml, Reason}}
    end.

-spec validate(term()) -> {ok, conf:apps_config()} | {error, error_reason()}.
validate(Y) ->
    case yval:validate(top_validator(), Y) of
        {ok, AppOpts} ->
            case create_validators(AppOpts) of
                {ok, Validators} ->
                    Validator = yval:options(Validators),
                    case yval:validate(Validator, AppOpts) of
                        {ok, Config} ->
                            {ok, Config};
                        {error, Reason, Ctx} ->
                            {error, {invalid_yaml_config, Reason, Ctx}}
                    end;
                {error, _} = Err ->
                    Err
            end;
        {error, Reason, Ctx} ->
            {error, {invalid_yaml_config, Reason, Ctx}}
    end.

-spec format_error(error_reason()) -> string().
format_error({unsupported_application, App}) ->
    "Erlang application '" ++ atom_to_list(App) ++ "' doesn't support YAML configuration";
format_error({invalid_yaml_config, {bad_enum, Known, Bad}, Ctx}) ->
    format_ctx(Ctx) ++
        format("Unexpected value: ~s. Did you mean '~s'? ~s",
               [Bad, best_match(Bad, Known),
                format_known("Possible values", Known)]);
format_error({invalid_yaml_config, {unknown_option, [], Opt}, Ctx}) ->
    format_ctx(Ctx) ++
        format("Unknown parameter: ~s. There are no available parameters", [Opt]);
format_error({invalid_yaml_config, {unknown_option, Known, Opt}, Ctx}) ->
    format_ctx(Ctx) ++
        format("Unknown parameter: ~s. Did you mean '~s'? ~s",
               [Opt, best_match(Opt, Known),
                format_known("Available parameters", Known)]);
format_error({invalid_yaml_config, Reason, Ctx}) ->
    yval:format_error(Reason, Ctx);
format_error({bad_yaml, Reason}) ->
    fast_yaml:format_error(Reason).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec create_validators([{atom(), term()}]) ->
                        {ok, yval:validators()} | {error, error_reason()}.
create_validators(AppOpts) ->
    lists:foldl(
      fun({App, _Opts}, {ok, Acc}) ->
              Mod = callback_module(App),
              case code:ensure_loaded(Mod) of
                  {module, Mod} ->
                      case erlang:function_exported(Mod, validator, 0) of
                          true ->
                              Validator = Mod:validator(),
                              {ok, Acc#{App => Validator}};
                          false ->
                              {error, {unsupported_application, App}}
                      end;
                  _ ->
                      {error, {unsupported_application, App}}
              end;
         (_, {error, _} = Err) ->
              Err
      end, {ok, #{}}, AppOpts).

top_validator() ->
    yval:map(yval:atom(), yval:any(), [unique]).

-spec callback_module(atom()) -> module().
callback_module(App) ->
    list_to_atom(atom_to_list(App) ++ "_yaml").

%%%===================================================================
%%% Formatters
%%%===================================================================
-spec format_ctx(yval:ctx()) -> string().
format_ctx([]) ->
    "";
format_ctx(Ctx) ->
    yval:format_ctx(Ctx) ++ ": ".

-spec format(iodata(), list()) -> string().
format(Fmt, Args) ->
    lists:flatten(io_lib:format(Fmt, Args)).

-spec format_known(string(), [atom() | binary() | string()]) -> iolist().
format_known(_, Known) when length(Known) > 20 ->
    "";
format_known(Prefix, Known) ->
    [Prefix, " are: ", format_join(Known)].

-spec format_join([atom() | string() | binary()]) -> string().
format_join([]) ->
    "(empty)";
format_join(L) ->
    Strings = lists:map(fun to_string/1, L),
    lists:join(", ", lists:sort(Strings)).

-spec best_match(atom() | binary() | string(),
                 [atom() | binary() | string()]) -> string().
best_match(Pattern, []) ->
    Pattern;
best_match(Pattern, Opts) ->
    String = to_string(Pattern),
    {Ds, _} = lists:mapfoldl(
                fun(Opt, Cache) ->
                        SOpt = to_string(Opt),
                        {Distance, Cache1} = ld(String, SOpt, Cache),
                        {{Distance, SOpt}, Cache1}
                end, #{}, Opts),
    element(2, lists:min(Ds)).

%% Levenshtein distance
-spec ld(string(), string(), distance_cache()) -> {non_neg_integer(), distance_cache()}.
ld([] = S, T, Cache) ->
    {length(T), maps:put({S, T}, length(T), Cache)};
ld(S, [] = T, Cache) ->
    {length(S), maps:put({S, T}, length(S), Cache)};
ld([X|S], [X|T], Cache) ->
    ld(S, T, Cache);
ld([_|ST] = S, [_|TT] = T, Cache) ->
    try {maps:get({S, T}, Cache), Cache}
    catch _:{badkey, _} ->
            {L1, C1} = ld(S, TT, Cache),
            {L2, C2} = ld(ST, T, C1),
            {L3, C3} = ld(ST, TT, C2),
            L = 1 + lists:min([L1, L2, L3]),
            {L, maps:put({S, T}, L, C3)}
    end.

-spec to_string(atom() | binary() | string()) -> string().
to_string(A) when is_atom(A) ->
    atom_to_list(A);
to_string(B) when is_binary(B) ->
    binary_to_list(B);
to_string(S) ->
    S.
