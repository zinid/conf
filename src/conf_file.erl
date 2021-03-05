%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(conf_file).

%% API
-export([path_to_ref/1]).
-export([read/2]).
-export([format_error/1]).
-export([format_ref/1]).
-export_type([ref/0, mime_type/0, error_reason/0]).

-type ref() :: binary() | uri_string:uri_map().
-type mime_type() :: binary().
-type error_reason() :: {file, file_error_reason()} |
                        {http, conf_http:error_reason()}.
-type file_error_reason() :: empty_path |
                             invalid_path |
                             unsupported_uri |
                             file:posix().

%%%===================================================================
%%% API
%%%===================================================================
-spec read(ref(), [mime_type(), ...]) -> {ok, term()} | {error, error_reason()}.
read(#{} = URI, Mimes) ->
    Opts = #{hdrs => [{'Accept', Mimes}]},
    case conf_http:get(URI, Opts) of
        {ok, _Hdrs, Data} ->
            {ok, Data};
        {error, Reason} ->
            {error, {http, Reason}}
    end;
read(Path, _) ->
    case file:read_file(Path) of
        {ok, Data} -> {ok, Data};
        {error, Reason} -> {error, {file, Reason}}
    end.

-spec path_to_ref(binary()) -> {ok, ref()} | {error, error_reason()}.
path_to_ref(<<>>) ->
    {error, {file, empty_path}};
path_to_ref(Path) ->
    case filename:pathtype(Path) of
        relative ->
            case path_to_uri(Path) of
                {ok, URI} -> {ok, URI};
                {error, relative_uri} ->
                    expand_path(Path);
                {error, Reason} ->
                    {error, {file, Reason}}
            end;
        _ ->
            expand_path(Path)
    end.

-spec format_error(error_reason()) -> string().
format_error({file, invalid_path}) ->
    "invalid URI or path";
format_error({file, unsupported_uri}) ->
    "unsupported URI scheme";
format_error({file, Reason}) ->
    case file:format_error(Reason) of
        "unknown POSIX error" = Text ->
            Text ++ ": " ++ atom_to_list(Reason);
        Text ->
            Text
    end;
format_error({http, Reason}) ->
    conf_http:format_error(Reason).

-spec format_ref(ref()) -> binary().
format_ref(#{} = URI) ->
    uri_string:normalize(URI);
format_ref(Path0) ->
    case expand_path(Path0) of
        {ok, Path} ->
            Path;
        {error, _Reason} ->
            Path0
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
path_to_uri(Path) ->
    case uri_string:parse(Path) of
        #{host := Host, scheme := <<"http">>} = URI when Host /= <<>> ->
            {ok, URI};
        #{scheme := _} ->
            {error, unsupported_uri};
        #{} ->
            {error, relative_uri};
        _ ->
            {error, invalid_path}
    end.

-spec expand_path(binary()) -> {ok, binary()} | {error, error_reason()}.
expand_path(Path) ->
    case lists:foldr(
           fun(_, error) -> error;
              (Part, Parts) ->
                   case expand_env(Part) of
                       {ok, Expanded} -> [Expanded|Parts];
                       error -> error
                   end
           end, [], filename:split(Path)) of
        error -> {error, {file, invalid_path}};
        SplitPath ->
            {ok, filename:absname(filename:join(SplitPath))}
    end.

-spec expand_env(binary()) -> {ok, binary()} | error.
expand_env(<<$$, Env/binary>>) ->
    case os:getenv(binary_to_list(Env)) of
        false -> {ok, <<>>};
        Chars ->
            try unicode:characters_to_binary(Chars) of
                Bin when is_binary(Bin) -> {ok, Bin};
                _ -> error
            catch _:_ ->
                    error
            end
    end;
expand_env(Other) ->
    {ok, Other}.
