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
%%% @doc Module implementing interface modules for torrenterl 
%%% See {@link download/1} function.
%%% @end
-module(torrenterl).
-behaviour(application).

%%API
-export([install/0, download/1, gen_peer_id/0]).
%% Callbacks
-export([start/2, stop/1]).

-include("torrenterl_types.hrl").

%% @hidden
-spec start(normal | {takeover, node()} | {failover, node()}, any()) ->
    {ok, pid()}.
start(_, _) ->
    case lists:member({seed,1}, ssl:module_info(exports)) of
        true ->
            % Make sure that the ssl random number generator is seeded
            % This was new in R13 (ssl-3.10.1 in R13B vs. ssl-3.10.0 in R12B-5)
            ssl:seed(crypto:rand_bytes(255));
        false ->
            ok
    end,
    torrenterl_sup:start_link().

%% @hidden
-spec stop(any()) -> ok.
stop(_) ->
    ok.

%% @spec () -> ok 
%% @doc Installs torrenterl database for persistent info.
%% Installs the tables which will hold the information which has to
%% be retained between restarts:
%% - Torrent information
%% - Torrent statistics
%% - Torrenterl configuration (the installer fills this with sane defaults) 
%% @end
-spec install() -> ok.
install() -> 
    ok = torrent_db:create_db(),
    ok = torrent_db:create_table(torrent),
    ok = torrent_db:create_table(torrent_stats),
    ok = torrent_db:create_table(torrent_config),
    ok = add_config_defaults().

-spec add_config_defaults() -> ok.
add_config_defaults() ->
    %%Only compact=1 is supported for now. Changing this option will
    %%most probably brake the code.
    ok = torrent_db:write(#torrent_config{key= <<"compact">>, value= <<"1">>}),
    ok = torrent_db:write(#torrent_config{key= <<"numwant">>, value= <<"50">>}),
    ok = filelib:ensure_dir("./Downloads"),
    ok = torrent_db:write(
     #torrent_config{key= download_dir, value= "./Downloads"}),
    ok = torrent_db:write(#torrent_config{key= listening_port, value= 51000}).

%% @spec (Path) -> Data
%%   Path = string()
%%   Data = binary | integer() | list()
%% @doc Decodes bencoded file.
%% Reads up file content and calls {@link decode/1} on it.
%% @end
-spec download(string()) -> ok.
download(File) ->
    {Data, _Rest} = bencode:decode_file(File),
    Torrent = lists:foldl(fun verify_torrent/2, #torrent{}, Data),
    TorrentStats = #torrent_stats{infohash= Torrent#torrent.infohash,
     left= Torrent#torrent.length},
    ok = torrent_db:write(Torrent),
    ok = torrent_db:write(TorrentStats),
    torrenterl_sup:start_torrent_manager(Torrent#torrent.infohash).

-spec verify_torrent(data(), record()) -> record().
verify_torrent({<<"announce">>, Url}, Torrent) ->
    Scrape = create_scrape_url(Url),
    Torrent#torrent{announce= binary_to_list(Url), scrape= Scrape};
verify_torrent({<<"info">>, FileInfo}, Torrent) ->
    InfoHash = crypto:sha(bencode:encode(FileInfo)),
    lists:foldl(fun verify_torrent_info/2, 
     Torrent#torrent{infohash= InfoHash}, FileInfo);
verify_torrent(_Else, Torrent) ->
    Torrent.

-spec create_scrape_url(binary()) -> string().
create_scrape_url(Url) ->
    List = re:split(Url, "(/)", [{return, list}]),
    [Announce | Rest] = lists:reverse(List),
    case re:replace(Announce, "^announce", "scrape", [{return, list}]) of
    Announce -> undefined;
    Scrape -> lists:flatten(lists:reverse([Scrape | Rest]))
    end.

-spec verify_torrent_info(data(), record()) -> record().
verify_torrent_info({<<"piece length">>, PLength}, 
 Torrent = #torrent{type= single, length= Length}) ->
    Torrent#torrent{piecelength= PLength, lastpiece= Length rem PLength};
verify_torrent_info({<<"pieces">>, Pieces}, Torrent) -> 
    HashList = to_hash_list(Pieces, []),
    Torrent#torrent{hashlist= HashList, nbofpieces= length(HashList)};
verify_torrent_info({<<"private">>, Num}, Torrent) -> 
    Torrent#torrent{private= Num};
verify_torrent_info({<<"name">>, Name}, Torrent) -> 
    Torrent#torrent{name= binary_to_list(Name)};
verify_torrent_info({<<"length">>, Length}, Torrent) -> 
    Torrent#torrent{type= single, length= Length};
verify_torrent_info({<<"files">>, _Files}, Torrent) ->
    Torrent#torrent{type= multi},
    exit(multi_file_not_supported); %%TODO: Add multi file support 
verify_torrent_info(_Else, Torrent) -> 
    Torrent.

-spec to_hash_list(binary(), list())-> [binary()].
to_hash_list(<<>>, Acc) ->
    lists:reverse(Acc);
to_hash_list(<<Hash:20/binary, Rest/binary>>, Acc) ->
    to_hash_list(Rest, [Hash, Acc]). 

-spec gen_peer_id() -> binary().
gen_peer_id() ->
    list_to_binary([<<"-ER0001-">>, 
     io_lib:format(
      "~2.16.0B~2.16.0B~2.16.0B~2.16.0B~2.16.0B~2.16.0B", 
      binary_to_list(crypto:rand_bytes(6)))]).

