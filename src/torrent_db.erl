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
%%% @doc Module implementing the database layer of torrenterl. 
%%% It is thin layer on top of mnesia for now, but it makes db migration
%%% possible/easy if it proves to be inadequate.
%%% @end
-module(torrent_db).

-export([create_db/0, create_table/1, ensure_loaded/1, list_torrents/0]).
-export([write/1, read/2]).

-include("torrenterl_types.hrl").

-spec create_db() -> ok.
create_db() ->
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok.

-spec create_table(atom()) -> ok.
create_table(Table) ->
    case Table of
    torrent ->
        {atomic, ok} = mnesia:create_table(torrent, 
            [{disc_copies, [node()]},
             {attributes, record_info(fields, torrent)}]);
    torrent_stats ->
        {atomic, ok} = mnesia:create_table(torrent_stats, 
            [{disc_copies, [node()]},
             {attributes, record_info(fields, torrent_stats)}]);
    torrent_config ->
        {atomic, ok} = mnesia:create_table(torrent_config, 
            [{disc_copies, [node()]},
             {attributes, record_info(fields, torrent_config)}])
    end,
    ok.

-spec ensure_loaded(list(atom())) -> ok.
ensure_loaded(Tables) ->
    ok = mnesia:wait_for_tables(Tables, infinity),
    ok.

-spec list_torrents() -> list().
list_torrents() ->
    mnesia:dirty_all_keys(torrent).

-spec write(record()) -> ok.
write(Record) ->
    {atomic, ok} = mnesia:transaction(fun () -> mnesia:write(Record) end),
    ok.

-spec read(atom(), term()) -> ok.
read(Table, Key) ->
    {atomic, [Record]} = 
     mnesia:transaction(fun () -> mnesia:read(Table, Key) end),
    Record.

