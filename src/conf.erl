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
-export([reload_file/0]).
-export([reload_file/1]).
-export([load/1]).
-export([reload/1]).
-export([get_path/0]).
-export([format_error/1]).
%% Application callbacks
-export([start/2, stop/1, config_change/3]).
-export_type([error_reason/0, apps_config/0]).

-type backend() :: conf_yaml_backend.
-type error_reason() :: {bad_env, conf_env:error_reason()} |
                        {bad_ref, conf_file:ref(), conf_file:error_reason()} |
                        {backend(), term()}.
-type apps_config() :: [{atom(), #{atom() => term()} | {atom(), term()}}].
-callback validator() -> yval:validator().

%%%===================================================================
%%% API
%%%===================================================================
-spec load_file(file:filename_all()) -> ok | {error, error_reason()}.
load_file(Path0) ->
    case prep_path(Path0) of
        {ok, Path} -> read_and_load_file(Path, false);
        error -> erlang:error(badarg, [Path0])
    end.

-spec reload_file() -> ok | {error, error_reason()}.
reload_file() ->
    case conf_env:file() of
        {ok, Path} ->
            reload_file(Path);
        {error, Reason} ->
            {error, {bad_env, Reason}}
    end.

-spec reload_file(file:filename_all()) -> ok | {error, error_reason()}.
reload_file(Path0) ->
    case prep_path(Path0) of
        {ok, Path} -> read_and_load_file(Path, true);
        error -> erlang:error(badarg, [Path0])
    end.

-spec load(term()) -> ok | {error, error_reason()}.
load(Y) ->
    load(Y, false).

-spec reload(term()) -> ok | {error, error_reason()}.
reload(Y) ->
    load(Y, true).

-spec get_path() -> {ok, binary()} | {error, error_reason()}.
get_path() ->
    case conf_env:file() of
        {ok, _} = OK -> OK;
        {error, Reason} -> {error, {bad_env, Reason}}
    end.

-spec format_error(error_reason()) -> string().
format_error({bad_env, Reason}) ->
    conf_env:format_error(Reason);
format_error({bad_ref, _Ref, Reason}) ->
    conf_file:format_error(Reason);
format_error({Module, Reason}) ->
    Module:format_error(Reason).

-spec start() -> ok | {error, term()}.
start() ->
    case application:ensure_all_started(?MODULE) of
        {ok, _} -> ok;
        {error, _} = Err -> Err
    end.

-spec stop() -> ok | {error, term()}.
stop() ->
    application:stop(?MODULE).

%%%===================================================================
%%% Application callbacks
%%%===================================================================
-spec start(normal | {takeover, node()} | {failover, node()}, term()) ->
          {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    case conf_env:file() of
        {ok, Path} ->
            case read_and_load_file(Path, false) of
                ok ->
                    conf_sup:start_link();
                {error, Reason} = Err ->
                    logger:critical(
                      "Failed to load configuration from ~ts: ~ts",
                      [conf_file:format_ref(Path), format_error(Reason)]),
                    do_stop(Err)
            end;
        {error, {undefined_env, _}} ->
            conf_sup:start_link();
        {error, Reason} = Err ->
            logger:critical("~s", [conf_env:format_error(Reason)]),
            do_stop(Err)
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
-spec read_and_load_file(binary(), boolean()) -> ok | {error, error_reason()}.
read_and_load_file(Path, Reload) ->
    case conf_file:path_to_ref(Path) of
        {error, Reason} -> {error, {bad_ref, Path, Reason}};
        {ok, Ref} ->
            Mimes = conf_yaml_backend:mime_types(),
            case conf_file:read(Ref, Mimes) of
                {error, Reason} ->
                    {error, {bad_ref, Ref, Reason}};
                {ok, Data} ->
                    case conf_yaml_backend:decode(Data) of
                        {ok, Y} ->
                            load(Y, Reload);
                        {error, Reason} ->
                            {error, {conf_yaml_backend, Reason}}
                    end
            end
    end.

-spec load(term(), boolean()) -> ok | {error, error_reason()}.
load(Y, Reload) ->
    case conf_yaml_backend:validate(Y) of
        {ok, Config} ->
            case conf_env:load(Config, Reload) of
                ok -> ok;
                {error, Errors} ->
                    report_config_change_errors(Errors)
            end;
        {error, Reason} ->
            {error, {conf_yaml_backend, Reason}}
    end.

-spec report_config_change_errors(term()) -> ok.
report_config_change_errors(Errors) when is_list(Errors) ->
    Errors1 = lists:filter(
                fun({module_not_defined, _}) -> false;
                   ({application_not_found, _}) -> false;
                   (_) -> true
                end, Errors),
    lists:foreach(
      fun(Error) ->
              logger:warning(
                "Failed to change configuration of Erlang application: ~p",
                [Error])
      end, Errors1);
report_config_change_errors(Error) ->
    logger:warning(
      "Failed to change configuration of Erlang applications: ~p",
      [Error]).

-spec do_stop({error, term()}) -> {error, term()}.
do_stop({error, Reason} = Err) ->
    case conf_env:on_fail() of
        stop ->
            Err;
        OnFail ->
            flush_logger(),
            Status = case OnFail of
                         crash -> format_error(Reason);
                         _Halt -> 1
                     end,
            halt(Status)
    end.

-spec flush_logger() -> ok.
flush_logger() ->
    lists:foreach(
      fun(#{id := Name, module := Mod}) ->
              case conf_misc:try_load(Mod, filesync, 1) of
                  {ok, _} -> Mod:filesync(Name);
                  _ -> ok
              end
      end, logger:get_handler_config()).

-spec prep_path(file:filename_all()) -> {ok, binary()} | error.
prep_path(Path) when is_binary(Path), Path /= <<>> ->
    {ok, Path};
prep_path(Path) when is_list(Path), Path /= [] ->
    try unicode:characters_to_binary(Path) of
        Bin when is_binary(Bin) -> {ok, Bin};
        _ -> error
    catch _:_ ->
            error
    end;
prep_path(_) ->
    error.
