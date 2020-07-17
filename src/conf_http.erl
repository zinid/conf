%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <xramtsov@gmail.com>
%%% @doc
%%%   Dead simple HTTP/1.1 client. This is not NIH.
%%%   The rationale is to avoid dependency bloat.
%%% @end
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
%%% Created : 16 Jul 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(conf_http).

%% API
-export([get/1, get/2]).
-export([format_error/1]).
-export_type([error_reason/0]).

-define(HTTP_TIMEOUT, timer:seconds(10)).
-define(BUF_SIZE, 65535).

-type hdrs() :: [{iodata(), iodata()}].
-type http_return() :: {non_neg_integer(), iodata(), binary()}.
-type error_reason() :: {http_error, http_error_reason()} | inet:posix().
-type socket() :: gen_tcp:socket().
-type http_error_reason() :: invalid_content_length |
                             invalid_chunk_size |
                             invalid_chunk_trailer |
                             string() | binary() |
                             {non_neg_integer(), iodata()}.

%%%===================================================================
%%% API
%%%===================================================================
-spec get(uri_string:uri_map()) -> {ok, binary()} | {error, error_reason()}.
get(URI) ->
    get(URI,
        case application:get_env(conf, http_timeout) of
            {ok, T} when is_integer(T), T>0 -> T;
            _ -> ?HTTP_TIMEOUT
        end).

-spec get(uri_string:uri_map(), non_neg_integer()) ->
                 {ok, binary()} | {error, error_reason()}.
get(#{host := Host, path := Path} = URI, Timeout) ->
    Hdrs = [{"Host", Host},
            {"Connection", "close"},
            {"Accept", "application/json, application/yaml, "
                       "application/x-yaml, text/x-yaml"},
            {"Content-Length", "0"}],
    DeadLine = deadline(Timeout),
    Port = maps:get(port, URI, 80),
    Host1 = if is_binary(Host) -> binary_to_list(Host);
               true -> Host
            end,
    case gen_tcp:connect(Host1, Port, [binary,
                                       {active, false},
                                       {packet, http_bin},
                                       {send_timeout_close, true}],
                         Timeout) of
        {ok, Socket} ->
            Query = case maps:find(query, URI) of
                        {ok, Q} -> [$?|Q];
                        error -> ""
                    end,
            Path1 = case Path of
                        "" -> [$/|Query];
                        <<>> -> [$/|Query];
                        _ -> [Path|Query]
                    end,
            case send_request(Socket, Path1, Hdrs, DeadLine) of
                {ok, {200, _, Body}} ->
                    {ok, Body};
                {ok, {Code, Status, _}} ->
                    {error, {http_error, {Code, Status}}};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec format_error(error_reason()) -> string().
format_error({http_error, Reason}) ->
    "HTTP error: " ++
        case Reason of
            invalid_content_length -> "invalid content length";
            invalid_chunk_size -> "invalid chunk size";
            invalid_chunk_trailer-> "invalid chunk trailer";
            {Code, Status} when Status == ""; Status == <<"">> ->
                "unexpected response code: " ++ integer_to_list(Code);
            {Code, Status} ->
                lists:flatten(io_lib:format("~s (~B)", [Status, Code]));
            _ ->
                lists:flatten(io_lib:format("~s", [Reason]))
        end;
format_error(timeout) ->
    format_error(etimedout);
format_error(Reason) ->
    case inet:format_error(Reason) of
        "unknown POSIX error" ->
            atom_to_list(Reason);
        Text ->
            Text
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%-------------------------------------------------------------------
%%% HTTP fuckery
%%%-------------------------------------------------------------------
-spec send_request(socket(), iolist(), hdrs(), non_neg_integer()) ->
                          {ok, http_return()} | {error, error_reason()}.
send_request(Socket, Path, Hdrs, DeadLine) ->
    Request = ["GET ", Path, " HTTP/1.1\r\n",
	       [[Hdr, ": ", Val, "\r\n"] || {Hdr, Val} <- Hdrs],
	       "\r\n"],
    Timeout = timeout(DeadLine),
    case inet:setopts(Socket, [{send_timeout, Timeout}]) of
        ok ->
            case gen_tcp:send(Socket, Request) of
                ok ->
                    recv_response(Socket, DeadLine);
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec recv_response(socket(), non_neg_integer()) ->
                           {ok, http_return()} | {error, error_reason()}.
recv_response(Sock, DeadLine) ->
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, 0, Timeout) of
	{ok, {http_response, _, Code, Status}} ->
	    recv_hdrs(Sock, DeadLine, Code, Status, 0, false);
	{ok, {http_error, _} = Err} ->
	    {error, Err};
	{error, _} = Err ->
	    Err
    end.

-spec recv_hdrs(socket(), non_neg_integer(), non_neg_integer(),
                iodata(), non_neg_integer(), boolean()) ->
                       {ok, http_return()} | {error, error_reason()}.
recv_hdrs(Sock, DeadLine, Code, Status, Size, Chunked) ->
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, 0, Timeout) of
	{ok, {http_header, _, 'Content-Length', _, Len}} ->
            try binary_to_integer(Len) of
                NewSize when NewSize >= 0 ->
                    recv_hdrs(Sock, DeadLine, Code, Status, NewSize, Chunked);
                _ ->
                    {error, {http_error, invalid_content_length}}
            catch _:badarg ->
                    {error, {http_error, invalid_content_length}}
            end;
	{ok, {http_header, _, 'Transfer-Encoding', _, <<"chunked">>}} ->
	    recv_hdrs(Sock, DeadLine, Code, Status, Size, true);
	{ok, {http_header, _, _, _, _}} ->
	    recv_hdrs(Sock, DeadLine, Code, Status, Size, Chunked);
	{ok, {http_error, _} = Err} ->
	    {error, Err};
	{ok, http_eoh} when Chunked ->
	    case inet:setopts(Sock, [{packet, line}]) of
                ok ->
                    case recv_chunk(Sock, DeadLine, <<>>) of
                        {ok, Body} -> {ok, {Code, Status, Body}};
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
	{ok, http_eoh} ->
	    case inet:setopts(Sock, [{packet, 0}]) of
                ok ->
                    case recv_body(Sock, DeadLine, Size, <<>>) of
                        {ok, Body} -> {ok, {Code, Status, Body}};
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
	{error, _} = Err ->
	    Err
    end.

-spec recv_chunk(socket(), non_neg_integer(), binary()) ->
                        {ok, binary()} | {error, error_reason()}.
recv_chunk(Sock, DeadLine, Body) ->
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, 0, Timeout) of
	{ok, Data} ->
	    try binary_to_integer(binary:part(Data, {0, size(Data)-2}), 16) of
		0 -> recv_trailer(Sock, DeadLine, 0, Body);
		Size when Size > 0 ->
		    case inet:setopts(Sock, [{packet, 0}]) of
                        ok ->
                            case recv_body(Sock, DeadLine, Size, Body) of
                                {ok, Body1} ->
                                    case inet:setopts(Sock, [{packet, line}]) of
                                        ok -> recv_trailer(Sock, DeadLine, Size, Body1);
                                        {error, _} = Err -> Err
                                    end;
                                {error, _} = Err ->
                                    Err
                            end;
                        {error, _} = Err ->
                            Err
                    end;
                _ ->
                    {error, {http_error, invalid_chunk_size}}
	    catch _:_ ->
		    {error, {http_error, invalid_chunk_size}}
	    end;
	{error, _} = Err ->
	    Err
    end.

-spec recv_trailer(socket(), non_neg_integer(), non_neg_integer(), binary()) ->
                          {ok, binary()} | {error, error_reason()}.
recv_trailer(Sock, DeadLine, Size, Body) ->
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, 0, Timeout) of
	{ok, <<"\r\n">>} ->
	    case Size of
		0 -> {ok, Body};
		_ -> recv_chunk(Sock, DeadLine, Body)
	    end;
	{ok, _} ->
            {error, {http_error, invalid_chunk_trailer}};
	{error, _} = Err ->
            Err
    end.

-spec recv_body(socket(), non_neg_integer(), non_neg_integer(), binary()) ->
                       {ok, binary()} | {error, inet:posix()}.
recv_body(_Sock, _DeadLine, 0, Body) ->
    {ok, Body};
recv_body(Sock, DeadLine, Size, Body) ->
    BufSize = min(Size, ?BUF_SIZE),
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, BufSize, Timeout) of
	{ok, Data} ->
	    recv_body(Sock, DeadLine, Size-size(Data),
		      <<Body/binary, Data/binary>>);
	{error, _} = Err ->
	    Err
    end.

%%%-------------------------------------------------------------------
%%% Aux crap
%%%-------------------------------------------------------------------
-spec deadline(non_neg_integer()) -> non_neg_integer().
deadline(Timeout) ->
    current_time() + Timeout.

-spec timeout(non_neg_integer()) -> non_neg_integer().
timeout(DeadLine) ->
    max(0, DeadLine - current_time()).

-spec current_time() -> non_neg_integer().
current_time() ->
    erlang:system_time(millisecond).
