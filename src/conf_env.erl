%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 27 Nov 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(conf_env).

%% API
-export([load/2]).
-export([file/0]).
-export([callback_module/1]).
-export([on_fail/0]).
-export([format_error/1]).
-export_type([error_reason/0]).

-type error_reason() :: {undefined_env, atom()} |
                        {invalid_env, atom(), term()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec load(conf:apps_config(), boolean()) -> ok | {error, _}.
load(Config, Reload) ->
    case Reload of
        false ->
            set_env(Config);
        true ->
            OldConfig = application_controller:prep_config_change(),
            set_env(Config),
            application_controller:config_change(OldConfig)
    end.

-spec file() -> {ok, binary()} | {error, error_reason()}.
file() ->
    case application:get_env(conf, file) of
        {ok, Path0} ->
            try unicode:characters_to_binary(Path0) of
                Path when is_binary(Path), Path /= <<>> ->
                    {ok, Path};
                _ ->
                    {error, {invalid_env, file, Path0}}
            catch _:_ ->
                    {error, {invalid_env, file, Path0}}
            end;
        undefined ->
            {error, {undefined_env, file}}
    end.

-spec on_fail() -> stop | crash | halt.
on_fail() ->
    case application:get_env(conf, on_fail, stop) of
        stop -> stop;
        crash -> crash;
        _ -> halt
    end.

-spec callback_module(atom()) -> {ok, module()} | {error, error_reason()}.
callback_module(App) ->
    Env = callback_yaml,
    case application:get_env(conf, Env) of
        {ok, AppMods} when is_list(AppMods) ->
            case lists:keyfind(App, 1, AppMods) of
                {_, Mod} when is_atom(Mod) ->
                    {ok, Mod};
                false ->
                    {error, {undefined_env, Env}};
                _ ->
                    {error, {invalid_env, Env, AppMods}}
            end;
        {ok, Junk} ->
            {error, {invalid_env, Env, Junk}};
        undefined ->
            {error, {undefined_env, Env}}
    end.

-spec format_error(error_reason()) -> string().
format_error({undefined_env, Env}) ->
    "Erlang environment variable '" ++ atom_to_list(Env) ++ "' is not set";
format_error({invalid_env, Env, Val}) ->
    lists:flatten(
      io_lib:format(
        "Invalid value of Erlang environment variable '~s': ~p",
        [Env, Val])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec set_env(conf:apps_config()) -> ok.
-ifdef(old_set_env). % Erlang/OTP < 21.3.
set_env(Config) ->
    lists:foreach(
      fun({App, Opts}) when is_map(Opts) ->
              maps:fold(
                fun(Par, Val, ok) ->
                        application:set_env(App, Par, Val, [{persistent, true}])
                end, ok, Opts);
         ({App, Opts}) when is_list(Opts) ->
              lists:foreach(
                fun({Par, Val}) ->
                        application:set_env(App, Par, Val, [{persistent, true}])
                end, Opts)
      end, Config).
-else.
set_env(Config) ->
    NewConfig = lists:map(
                  fun({App, Opts}) when is_map(Opts) ->
                          {App, maps:to_list(Opts)};
                     ({App, Opts}) when is_list(Opts) ->
                          {App, Opts}
                  end, Config),
    application:set_env(NewConfig, [{persistent, true}]).
-endif.
