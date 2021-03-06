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
-module(ssl_yaml).

-behaviour(conf).

%% API
-export([validator/0]).
%% Imported validators
-import(yval, [bool/0, enum/1, beam/0, timeout/1, list/1, options/2,
               pos_int/0, int/2, term/0, and_then/2, list_or_single/2,
               any/0]).

%%%===================================================================
%%% API
%%%===================================================================
-spec validator() -> yval:validator().
validator() ->
    options(
      #{protocol_version =>
            list_or_single(
              enum(['tlsv1.3', 'tlsv1.2', 'tlsv1.1', tlsv1, sslv3]),
              [unique]),
        dtls_protocol_version =>
            list_or_single(enum(['dtlsv1.2', dtlsv1]), [unique]),
        session_lifetime => conf_misc:to_seconds(timeout(second)),
        session_cb => beam(),
        session_cb_init_args => and_then(term(), list(any())),
        session_cache_client_max => pos_int(),
        session_cache_server_max => pos_int(),
        ssl_pem_cache_clean => timeout(millisecond),
        bypass_pem_cache => bool(),
        alert_timeout => timeout(millisecond),
        internal_active_n => int(-32768, 32767)},
      [unique]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
