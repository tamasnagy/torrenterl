%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Tamas Nagy <tamas@erlang-consulting.com>
%%% @doc Module implementing top level supervisor for torrenterl 
%%% See {@link download/1} function.
%%% @end
-module(torrenterl_sup).
-behaviour(supervisor).

%%API
-export([start_link/0, start_torrent_manager/1]).
%%Callback
-export([init/1]).

-type child() :: {atom(), {atom(), atom(), list(any)},
    atom(), integer(), atom(), list(atom())}.

%% @spec () -> {ok, pid()} | {error, Reason}
%% Reason = atom()
%% @doc Starts and links to the supervisor.
%% This is normally called from an application behaviour or from another
%% supervisor.
%% @end
-spec start_link() -> {ok, pid()} | {error, atom()}.
start_link() ->
    io:format("Loading torrent information...", []),
    torrent_db:ensure_loaded([torrent, torrent_stats, torrent_config]),
    io:format("Done.~n", []),
    supervisor:start_link({local, ?MODULE}, 
     ?MODULE, torrent_db:list_torrents()).

%%TODO: return type
-spec start_torrent_manager(binary()) -> any().
start_torrent_manager(Hash) -> 
    Manager = {{torrent_manager, Hash}, 
     {torrent_manager, start_link, [Hash]},
     permanent, 10000, worker, [torrent_manager]},
    case supervisor:start_child(?MODULE, Manager) of
    {ok, _} -> ok;
    {ok, _, _} -> ok;
    Else -> Else
    end.

%% @hidden
-spec init(any()) -> {ok, {{atom(), integer(), integer()}, [child()]}}.
init(Hashes) ->
    Managers = lists:map(fun (Hash) ->
    {{torrent_manager, Hash}, {torrent_manager, start_link, [Hash]},
        permanent, 10000, worker, [torrent_manager]} end, Hashes),
    {ok, {{one_for_one, 10, 1}, Managers}}.
