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
%%% @doc Module implementing torrenterl manager 
%%% See {@link download/1} function.
%%% @end
-module(torrent_manager).

-export([download/1]).

-include("torrenterl_types.hrl").

-record(torrent, {
      announce
    , infohash
    , piecelength
    , hashlist
    , lastpiece
    , nbofpieces
    , private= 0
    , type
    , name
    , length
    , peerid= gen_peer_id()
    , trackerid  
    }).

-record(torrent_stats,{
      infohash
    , uploaded= 0
    , downloaded= 0 
    , left
    }).

%%TODO: Options: Download directory, listening port,

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
    send_announce(Torrent, TorrentStats, [{<<"event">>, <<"started">>},
     {<<"compact">>, <<"1">>}, %%NOTE: Only support compact response for now.
     {<<"numwant">>, <<"50">>}]). %%NOTE: First ask for 50 this might change

-spec verify_torrent(data(), record()) -> record().
verify_torrent({<<"announce">>, Url}, Torrent) ->
    Torrent#torrent{announce= binary_to_list(Url)};
verify_torrent({<<"info">>, FileInfo}, Torrent) ->
    InfoHash = crypto:sha(bencode:encode(FileInfo)),
    lists:foldl(fun verify_torrent_info/2, 
     Torrent#torrent{infohash= InfoHash}, FileInfo);
verify_torrent(_Else, Torrent) ->
    Torrent.

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

-spec send_announce(record(), record(), list()) -> tuple().
send_announce(Torrent = #torrent{announce= Announce}, TorrentStats, Params) ->
    TorrentParams = to_announce_params(Torrent),
    TorrentStatParams = to_announce_params(TorrentStats),
    ParamList  = 
     create_get_param_list(TorrentParams ++ TorrentStatParams ++ Params),
    lhttpc:request(lists:flatten(Announce ++ ParamList), "GET", [], 5000).
    
-spec to_announce_params(record()) -> list(tuple()).
to_announce_params(#torrent{infohash= InfoHash, peerid= PeerId}) ->
    [{<<"info_hash">>, list_to_binary(edoc_lib:escape_uri(
     binary_to_list(InfoHash)))},
     {<<"peer_id">>, PeerId}];
to_announce_params(#torrent_stats{
 uploaded= Up, downloaded= Down, left= Left}) ->
    [{<<"uploaded">>, list_to_binary(integer_to_list(Up))},
     {<<"downloaded">>, list_to_binary(integer_to_list(Down))},
     {<<"left">>, list_to_binary(integer_to_list(Left))}].
     
create_get_param_list(List) ->
    "?" ++ string:join([ [binary_to_list(Key), "=", binary_to_list(Value)]  || 
        {Key, Value} <- List], "&").

gen_peer_id() ->
    list_to_binary([<<"-ER0001-">>, 
     list_to_binary(io_lib:format("~.12.0w",[random:uniform(999999999999)]))]).
