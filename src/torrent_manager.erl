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

-record(torrent, {announce, info_hash}).
%% @spec (Path) -> Data
%%   Path = string()
%%   Data = binary | integer() | list()
%% @doc Decodes bencoded file.
%% Reads up file content and calls {@link decode/1} on it.
%% @end
-spec download(string()) -> ok.
download(File) ->
    {Data, _Rest} = bencode:decode_file(File),
    verify_torrent(Data, #torrent{}).

-spec verify_torrent(data(), record()) -> record().
verify_torrent([{<<"announce", Url>>} | Rest], Torrent) ->
    verify_torrent(Rest, Torrent#torrent{announce= Url});
verify_torrent([{<<"info">>, FileInfo} | Rest], Torrent) ->
    verify_torrent_info(FileInfo, Torrent). %%TODO: info_hash

%%TODO: analyse one or more files content info
-spec verify_torrent_info(data(), record()) -> record().
verify_torrent_info(_, Torrent) -> Torrent.

