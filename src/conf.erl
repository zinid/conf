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
-module(conf).

-behaviour(application).

%% API
-export([start/0]).
-export([stop/0]).
-export([load_file/1]).
%% Application callbacks
-export([start/2, stop/1, config_change/3]).

-type error_reason() :: {unsupported_application, atom()}.
-type config() :: [{atom(), #{atom() => term()}}].

-callback validator() -> yval:validator().

%%%===================================================================
%%% API
%%%===================================================================
-spec load_file(file:filename_all()) -> ok | {error, error_reason() | yval:error_reason()}.
load_file(Path) ->
    case read_file(expand_path(Path)) of
        {ok, Config} ->
            load_config(Config);
        {error, _} = Err ->
            Err
    end.

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok.
stop() ->
    application:stop(?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
          {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case application:get_env(conf, file) of
        {ok, Path} ->
            case load_file(Path) of
                ok ->
                    conf_sup:start_link();
                Err ->
                    Err
            end;
        undefined ->
            conf_sup:start_link()
    end.

-spec stop(term()) -> ok.
stop(_State) ->
    ok.

-spec config_change(Changed :: [{atom(), term()}],
                    New :: [{atom(), term()}],
                    Removed :: [atom()]) -> ok.
config_change(_Changed, _New, _Removed) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec format_error(error_reason()) -> string().
format_error({unsupported_application, App}) ->
    "Erlang application '" ++ atom_to_list(App) ++ "' doesn't support YAML configuration".

-spec read_file(file:filename_all()) -> {ok, config()} | {error, error_reason() | yval:error_reason()}.
read_file(Path) ->
    case fast_yaml:decode_from_file(Path) of
        {ok, [Y]} ->
            case yval:validate(top_validator(), Y) of
                {ok, AppOpts} ->
                    case create_validators(AppOpts) of
                        {ok, Validators} ->
                            Validator = yval:options(Validators),
                            case yval:validate(Validator, AppOpts) of
                                {ok, Config} ->
                                    {ok, Config};
                                {error, Reason, Ctx} ->
                                    logger:critical("Failed to load configuration from ~ts: ~ts",
                                                    [Path, yval:format_error(Reason, Ctx)]),
                                    {error, Reason}
                            end;
                        {error, Reason} = Err ->
                            logger:critical("Failed to load configuration from ~ts: ~s",
                                            [Path, format_error(Reason)]),
                            Err
                    end;
                {error, Reason, Ctx} ->
                    logger:critical("Failed to load configuration from ~ts: ~ts",
                                    [Path, yval:format_error(Reason, Ctx)]),
                    {error, Reason}
            end;
        {ok, []} ->
            {ok, []};
        {error, Reason} = Err ->
            logger:critical("Failed to read YAML file ~ts: ~s",
                            [Path, fast_yaml:format_error(Reason)]),
            Err
    end.

-spec load_config(config()) -> ok.
load_config(Config) ->
    NewConfig = lists:map(
                  fun({App, Opts}) when is_map(Opts) ->
                          {App, maps:to_list(Opts)};
                     ({App, Opts}) when is_list(Opts) ->
                          {App, Opts}
                  end, Config),
    application:set_env(NewConfig, [{persistent, true}]).

-spec create_validators([{atom(), term()}]) ->
                        {ok, yval:validators()} | {error, error_reason()}.
create_validators(AppOpts) ->
    lists:foldl(
      fun({App, _Opts}, {ok, Acc}) ->
              Mod = callback_module(App),
              case code:ensure_loaded(Mod) of
                  {module, Mod} ->
                      Validator = Mod:validator(),
                      {ok, Acc#{App => Validator}};
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

-spec expand_path(file:filename_all()) -> file:filename_all().
expand_path(Path) ->
    filename:absname(
      filename:join(
        lists:map(fun expand_env/1, filename:split(Path)))).

-spec expand_env(unicode:chardata()) -> unicode:chardata().
expand_env(<<$$, _/binary>> = Env) ->
    expand_env(binary_to_list(Env));
expand_env([$$|Env]) ->
    case os:getenv(Env) of
        false -> [$$|Env];
        Value -> Value
    end;
expand_env(Other) ->
    Other.
