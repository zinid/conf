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
%%% Created : 16 Jul 2020 by Evgeny Khramtsov <xramtsov@gmail.com>
%%%-------------------------------------------------------------------
-module(conf_http).

%% API
-export([get/1, get/2]).
-export([find_hdr/2]).
-export([format_error/1]).
-export_type([req_hdr/0, resp_hdr/0, error_reason/0]).

-type options() :: #{timeout => non_neg_integer(),
                     hdrs => [req_hdr()]}.
-type req_hdr() :: {atom(), term()} | {iodata(), iodata()}.
-type resp_hdr() :: {atom() | string() | binary(), string() | binary()}.
-type http_return() :: {non_neg_integer(), iodata(), [resp_hdr()], binary()}.
-type error_reason() :: http_return() | {http_error, http_error_reason()} | inet:posix().
-type socket() :: gen_tcp:socket().
-type http_error_reason() :: invalid_content_length |
                             invalid_chunk_size |
                             invalid_chunk_trailer |
                             {invalid_mime_type, binary()} |
                             {invalid_location, binary()} |
                             string() | binary().

%%%===================================================================
%%% API
%%%===================================================================
-spec get(uri_string:uri_map()) -> {ok, [resp_hdr()], binary()} | {error, error_reason()}.
get(URI) ->
    get(URI, #{}).

-spec get(uri_string:uri_map(), options()) ->
                 {ok, [resp_hdr()], binary()} | {error, error_reason()}.
get(#{host := Host} = URI, Opts) when Host /= "", Host /= <<>> ->
    Host1 = if is_binary(Host) -> binary_to_list(Host);
               true -> Host
            end,
    Port = maps:get(port, URI, 80),
    Timeout = case maps:find(timeout, Opts) of
                  {ok, T} -> T;
                  error -> default_timeout()
              end,
    DeadLine = deadline(Timeout),
    case gen_tcp:connect(Host1, Port, [binary,
                                       {active, false},
                                       {packet, http_bin},
                                       {send_timeout_close, true}],
                         Timeout) of
        {ok, Socket} ->
            ReqHdrs = maps:get(hdrs, Opts, []),
            Resp = send_request(Socket, URI, ReqHdrs, DeadLine),
            _ = gen_tcp:close(Socket),
            case Resp of
                {ok, {200, _, RespHdrs, Body}} ->
                    check_headers(ReqHdrs, RespHdrs, Body);
                {ok, {Code, _, _, _} = Ret} when Code >= 300, Code < 400 ->
                    redirect(Ret, DeadLine, URI, Opts);
                {ok, {_, _, _, _} = Ret} ->
                    {error, Ret};
                {error, _} = Err ->
                    Err
            end;
        {error, _} = Err ->
            Err
    end.

-spec find_hdr(atom(), [resp_hdr()]) -> {ok, term()} | {error, notfound}.
find_hdr(Hdr, Hdrs) ->
    case lists:keyfind(Hdr, 1, Hdrs) of
        false -> {error, notfound};
        Found -> {ok, decode_hdr(Found)}
    end.

-spec format_error(error_reason()) -> string().
format_error({http_error, Reason}) ->
    "HTTP error: " ++
        case Reason of
            invalid_content_length -> "invalid content length";
            invalid_chunk_size -> "invalid chunk size";
            invalid_chunk_trailer-> "invalid chunk trailer";
            {invalid_mime_type, Mime} ->
                "unexpected MIME type: " ++ binary_to_list(Mime);
            {invalid_location, Location} ->
                "redirected to unsupported URI: " ++ binary_to_list(Location);
            _ ->
                lists:flatten(io_lib:format("~s", [Reason]))
        end;
format_error({Code, Status, _, _}) when Status == ""; Status == <<"">> ->
    "unexpected response code: " ++ integer_to_list(Code);
format_error({Code, Status, _, _}) ->
    lists:flatten(io_lib:format("~s (~B)", [Status, Code]));
format_error(timeout) ->
    format_error(etimedout);
format_error(closed) ->
    format_error(econnreset);
format_error(Reason) ->
    case inet:format_error(Reason) of
        "unknown POSIX error" = Text ->
            Text ++ ": " ++ atom_to_list(Reason);
        Text ->
            Text
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
%%%-------------------------------------------------------------------
%%% HTTP fuckery
%%%-------------------------------------------------------------------
-spec send_request(socket(), uri_string:uri_map(), [req_hdr()], non_neg_integer()) ->
                          {ok, http_return()} | {error, error_reason()}.
send_request(Socket, #{host := Host} = URI, ReqHdrs, DeadLine) ->
    Query = case maps:get(query, URI, "") of
                "" -> "";
                <<>> -> <<>>;
                Q -> [$?|Q]
            end,
    Path = case maps:get(path, URI, "") of
               "" -> $/;
               <<>> -> $/;
               P -> P
           end,
    Request = ["GET ", Path, Query, " HTTP/1.1\r\n",
               "Host: ", Host, "\r\n",
               "Connection: close\r\n",
               "Content-Length: 0\r\n",
	       [[encode_hdr(Hdr), "\r\n"] || Hdr <- ReqHdrs],
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
	    recv_hdrs(Sock, DeadLine, Code, Status, 0, [], false);
	{ok, {http_error, _} = Err} ->
	    {error, Err};
	{error, _} = Err ->
	    Err
    end.

-spec recv_hdrs(socket(), non_neg_integer(), non_neg_integer(),
                iodata(), non_neg_integer(), [resp_hdr()], boolean()) ->
                       {ok, http_return()} | {error, error_reason()}.
recv_hdrs(Sock, DeadLine, Code, Status, Size, Hdrs, Chunked) ->
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, 0, Timeout) of
	{ok, {http_header, _, 'Content-Length' = Hdr, _, Val}} ->
            try binary_to_integer(Val) of
                Len when Len >= 0 ->
                    recv_hdrs(Sock, DeadLine, Code, Status,
                              Len, [{Hdr, Val}|Hdrs], Chunked);
                _ ->
                    {error, {http_error, invalid_content_length}}
            catch _:badarg ->
                    {error, {http_error, invalid_content_length}}
            end;
	{ok, {http_header, _, 'Transfer-Encoding' = Hdr, _, <<"chunked">> = Val}} ->
	    recv_hdrs(Sock, DeadLine, Code, Status, Size, [{Hdr, Val}|Hdrs], true);
	{ok, {http_header, _, Hdr, _, Val}} ->
	    recv_hdrs(Sock, DeadLine, Code, Status, Size, [{Hdr, Val}|Hdrs], Chunked);
	{ok, {http_error, _} = Err} ->
	    {error, Err};
	{ok, http_eoh} when Chunked ->
	    case inet:setopts(Sock, [{packet, line}]) of
                ok ->
                    case recv_chunk(Sock, DeadLine, <<>>) of
                        {ok, Body} -> {ok, {Code, Status, Hdrs, Body}};
                        {error, _} = Err -> Err
                    end;
                {error, _} = Err ->
                    Err
            end;
	{ok, http_eoh} ->
	    case inet:setopts(Sock, [{packet, 0}]) of
                ok ->
                    case recv_body(Sock, DeadLine, Size, <<>>) of
                        {ok, Body} -> {ok, {Code, Status, Hdrs, Body}};
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
    BufSize = min(Size, 65535),
    Timeout = timeout(DeadLine),
    case gen_tcp:recv(Sock, BufSize, Timeout) of
	{ok, Data} ->
	    recv_body(Sock, DeadLine, Size-size(Data),
		      <<Body/binary, Data/binary>>);
	{error, _} = Err ->
	    Err
    end.

-spec check_headers([req_hdr()], [resp_hdr()], binary()) ->
                           {ok, [resp_hdr()], binary()} | {error, error_reason()}.
check_headers(ReqHdrs, RespHdrs, Body) ->
    case lists:keyfind('Accept', 1, ReqHdrs) of
        false -> {ok, RespHdrs, Body};
        {_, Mimes} ->
            case find_hdr('Content-Type', RespHdrs) of
                {error, _} -> {ok, RespHdrs, Body};
                {ok, Mime} ->
                    case lists:member(Mime, Mimes) of
                        true -> {ok, RespHdrs, Body};
                        false -> {error, {http_error, {invalid_mime_type, Mime}}}
                    end
            end
    end.

-spec redirect(http_return(), non_neg_integer(), uri_string:uri_map(), options()) ->
                      {ok, [resp_hdr()], binary()} | {error, error_reason()}.
redirect({Code, _, RespHdrs, _} = Ret, DeadLine, URI, Opts)
  when (Code >= 300 andalso Code < 304) orelse Code == 307 ->
    case lists:keyfind('Location', 1, RespHdrs) of
        false -> {error, Ret};
        {_, Location} ->
            %% TODO: limit the number of redirections
            case resolve(Location, URI) of
                {ok, RedirURI} ->
                    get(RedirURI, Opts#{timeout => timeout(DeadLine)});
                error ->
                    {error, {http_error, {invalid_location, Location}}}
            end
    end;
redirect(Ret, _, _, _) ->
    {error, Ret}.

-spec resolve(binary(), uri_string:uri_map()) ->
                     {ok, uri_string:uri_map()} | error.
%% uri_string:resolve/2 was only introduced in OTP 22.3
%% So we use this hand-crafted version
resolve(Location, BaseURI) ->
    case uri_string:parse(Location) of
        #{host := Host, scheme := <<"http">>} = URI when Host /= <<>> ->
            %% Absolute HTTP URI
            {ok, URI};
        #{scheme := Scheme} when Scheme /= <<>> ->
            %% Absolute non-HTTP URI
            error;
        #{path := Path} = URI when Path /= <<>> ->
            %% Relative URI
            #{host := Host, scheme := Scheme} = BaseURI,
            {ok, URI#{host => Host, scheme => Scheme}};
        _ ->
            error
    end.

%%%-------------------------------------------------------------------
%%% Headers codec
%%%-------------------------------------------------------------------
-spec decode_hdr({atom(), binary()}) -> term().
decode_hdr({'Content-Length', Val}) -> binary_to_integer(Val);
decode_hdr({'Content-Type', Val}) -> decode_content_type(Val);
decode_hdr({'Cache-Control', Val}) -> decode_cache_control(Val);
decode_hdr({Hdr, Val}) when Hdr == 'Expires'; Hdr == 'Date' ->
    decode_date(Val);
decode_hdr({'Age', Val}) ->
    decode_age(Val);
decode_hdr({_, Val}) ->
    Val.

-spec encode_hdr({atom() | binary() | string(), term()}) -> iolist().
encode_hdr({'Accept', Mimes}) ->
    encode_hdr({"Accept", lists:join(", ", Mimes)});
encode_hdr({Hdr, Val}) when is_atom(Hdr) ->
    encode_hdr({atom_to_list(Hdr), Val});
encode_hdr({Hdr, Val}) ->
    [Hdr, ": ", Val].

-spec decode_cache_control(binary()) -> [binary() | {'max-age', non_neg_integer()}].
decode_cache_control(Data) ->
    lists:map(
      fun(Val) ->
              case decode_token(Val) of
                  {<<"max-age">>, <<$=, Age/binary>>} ->
                      {'max-age', decode_age(Age)};
                  _ ->
                      Val
              end
      end, split_tokens(Data)).

-spec decode_age(binary()) -> non_neg_integer().
decode_age(Data) ->
    try binary_to_integer(Data) of
        Age when Age>=0 -> Age;
        _ -> 0
    catch _:_ ->
            0
    end.

-spec decode_content_type(binary()) -> binary().
decode_content_type(Data) ->
    case re:run(Data, [$^, token_regexp(), $/, token_regexp()]) of
        {match, [Part]} ->
            string:lowercase(binary:part(Data, Part));
        nomatch ->
            Data
    end.

-spec decode_date(binary()) -> calendar:datetime().
decode_date(<<_:3/binary, ", ",
             Day:2/binary, " ", Month:3/binary, " ", Year:4/binary, " ",
             Hour:2/binary, ":", Min:2/binary, ":", Sec:2/binary, " GMT">>) ->
    try {{binary_to_integer(Year), decode_month(Month), binary_to_integer(Day)},
         {binary_to_integer(Hour), binary_to_integer(Min), binary_to_integer(Sec)}} of
        DateTime ->
            case valid_datetime(DateTime) of
                true -> DateTime;
                false -> {{0, 1, 1}, {0, 0, 0}}
            end
    catch _:_ ->
            {{0, 1, 1}, {0, 0, 0}}
    end;
decode_date(_) ->
    %% TODO: decode obsolete formats
    {{0, 1, 1}, {0, 0, 0}}.

-spec decode_month(binary()) -> 1..12.
decode_month(Month) ->
    case Month of
        <<"Jan">> -> 1;
        <<"Feb">> -> 2;
        <<"Mar">> -> 3;
        <<"Apr">> -> 4;
        <<"May">> -> 5;
        <<"Jun">> -> 6;
        <<"Jul">> -> 7;
        <<"Aug">> -> 8;
        <<"Sep">> -> 9;
        <<"Oct">> -> 10;
        <<"Nov">> -> 11;
        <<"Dec">> -> 12
    end.

-spec valid_datetime(calendar:datetime()) -> boolean().
valid_datetime({Date, {H, M, S}}) ->
    calendar:valid_date(Date) andalso
        H>=0 andalso H=<23 andalso M>=0 andalso M=<59 andalso S>=0 andalso S=<60.

-spec split_tokens(binary()) -> [binary()].
split_tokens(Data) ->
    re:split(Data, "[ \t]*,[ \t]*").

-spec decode_token(binary()) -> {binary(), binary()} | nomatch.
decode_token(Data) ->
    case re:run(Data, "^" ++ token_regexp()) of
        {match, [{0, Size}]} ->
            <<Token:Size/binary, Rest/binary>> = Data,
            {string:lowercase(Token), Rest};
        nomatch ->
            nomatch
    end.

token_regexp() ->
    "[!#$%&'*+-.^_`|~0-9a-zA-Z]+".

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

-spec default_timeout() -> non_neg_integer().
default_timeout() ->
    case application:get_env(conf, http_timeout) of
        {ok, T} when is_integer(T), T>0 -> T;
        _ -> timer:seconds(10)
    end.
