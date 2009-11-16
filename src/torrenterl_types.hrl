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

-type b_dict() :: binary().
-type b_list() :: binary().
-type b_integer() :: binary().
-type b_string() :: binary().

-type b_data() :: b_dict() | b_list() | b_integer() | b_string().

-type erstring() :: binary().
-type erdict() :: [{erstring(), list() | integer() | erstring()}].

-type data() :: erdict() | list() | integer() | erstring().

-type hash() :: binary().

-record(torrent, {
      infohash
    , announce
    , piecelength
    , hashlist
    , lastpiece
    , nbofpieces
    , private= 0
    , type
    , name
    , length
    , peerid= torrenterl:gen_peer_id()
    , trackerid
    }).

-record(torrent_stats,{
      infohash
    , uploaded= 0
    , downloaded= 0 
    , left
    , state= start
    }).

-record(torrent_config,{
      key
    , value
    }).

