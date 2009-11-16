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
%%% See {@link start_link/1} function.
%%% @end
-module(torrent_manager).
-behaviour(gen_server).

%%API
-export([
        start_link/1
    ]).

%%Callback
-export([
        init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        code_change/3,
        terminate/2
    ]).

-include("torrenterl_types.hrl").

-record(state, {
     hash
    ,interval
    ,min_interval
    ,complete
    ,incomplete
    ,timer
    ,peers
    ,running_peers 
    ,numwant = torrent_db:read(torrent_config, <<"numwant">>) 
    }).

%% @spec (Hash) -> {ok, pid()}
%%   Hash = binary()
%% @doc Starts and link to the gen server.
%% This is normally called by a supervisor.
%% @end
-spec start_link(hash()) -> {ok, pid()} | {error, already_started}.
start_link(Hash) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Hash], []).

%% @hidden
-spec init(any()) -> {ok, #state{}}.
init([Hash]) ->
    State = send_announce(#state{hash= Hash}),
    {ok, State}.

%% @hidden
-spec handle_call(any(), any(), #state{}) ->
    {reply, any(), #state{}}.
handle_call(_, _, State) ->
    {reply, {error, unknown_request}, State}.

%% @hidden
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast(_, State) ->
    {noreply, State}.

%% @hidden
-spec handle_info(any(), #state{}) -> {noreply, #state{}}.
handle_info({timeout, Tref, announce}, State = #state{timer= Tref}) ->
    NewState = send_announce(State),
    {noreply, NewState};
handle_info(_, State) ->
    {noreply, State}.

%% @hidden
-spec terminate(any(), #state{}) -> ok.
terminate(_, _State) ->
    ok.

%% @hidden
-spec code_change(any(), #state{}, any()) -> #state{}.
code_change(_, State, _) ->
    State.

-spec send_announce(#state{}) -> #state{}.
send_announce(State= #state{hash= Hash}) ->
    Torrent = torrent_db:read(torrent, Hash), 
    TorrentStats = torrent_db:read(torrent_stats, Hash), 
    {ok, {_Status, _Header, Body}} = 
     send_announce(Torrent, TorrentStats, State), 
    ResponseDict = bencode:decode(Body),
    {NewState, NewTorrent, NewTorrentStats} =  
     process_response(ResponseDict, #state{}, Torrent, TorrentStats),
    torrent_db:write(NewTorrent),
    torrent_db:write(NewTorrentStats),
    start_timer(NewState).

-spec send_announce(#torrent{}, #torrent_stats{}, #state{}) -> tuple().
send_announce(Torrent = #torrent{announce= Announce}, TorrentStats, State) ->
    TorrentParams = to_announce_params(Torrent),
    TorrentStatParams = to_announce_params(TorrentStats),
    OptionParams = to_announce_params(State),
    ParamList  = 
     create_get_param_list(TorrentParams ++ TorrentStatParams ++ OptionParams),
    lhttpc:request(lists:flatten(Announce ++ ParamList), "GET", [], 5000).
    
-spec to_announce_params(#torrent{} | #torrent_stats{} | #state{}) -> 
 list({binary(), binary()}).
to_announce_params(#torrent{infohash= InfoHash, peerid= PeerId, 
 trackerid= TrackerId}) ->
    [{<<"info_hash">>, list_to_binary(escape_uri(
        binary_to_list(InfoHash), []))},
     {<<"peer_id">>, PeerId}
    | to_announce_params_trackerid(TrackerId)];
to_announce_params(#torrent_stats{
 uploaded= Up, downloaded= Down, left= Left, state= State}) ->
    [{<<"uploaded">>, list_to_binary(integer_to_list(Up))},
     {<<"downloaded">>, list_to_binary(integer_to_list(Down))},
     {<<"left">>, list_to_binary(integer_to_list(Left))}
     | to_announce_params_state(State)];
to_announce_params(#state{numwant= Numwant}) ->
    [{<<"compact">>, torrent_db:read(torrent_config, <<"compact">>)},
     {<<"numwant">>, Numwant}]. 

-spec to_announce_params_state(atom()) -> [{binary(), binary()}].
to_announce_params_state(start) ->
    [{<<"event">>, <<"started">>}];
to_announce_params_state(complete) ->
    [{<<"event">>, <<"completed">>}];
to_announce_params_state(stop) ->
    [{<<"event">>, <<"stopped">>}];
to_announce_params_state(progressing) ->
    [].

-spec to_announce_params_trackerid(atom()) -> [{binary(), binary()}].
to_announce_params_trackerid(undefined) ->
    [];
to_announce_params_trackerid(TrackerId) ->
    [{<<"trackerid">>, TrackerId}].

-spec create_get_param_list([{binary(), binary()}]) -> iolist(). 
create_get_param_list(List) ->
    "?" ++ string:join([ [binary_to_list(Key), "=", binary_to_list(Value)]  || 
        {Key, Value} <- List], "&").

escape_uri([], Acc) ->
    lists:reverse(Acc);
escape_uri([Char | Rest], Acc)
 when Char >= $a, Char =< $z ->
    escape_uri(Rest, [Char | Acc]);
escape_uri([Char | Rest], Acc)
 when Char >= $A, Char =< $Z ->
    escape_uri(Rest, [Char | Acc]);
escape_uri([Char | Rest], Acc)
 when Char >= $0, Char =< $9 ->
    escape_uri(Rest, [Char | Acc]);
escape_uri([Char | Rest], Acc)
 when Char == $.; Char == $-; Char == $_; Char == $~ ->
    escape_uri(Rest, [Char | Acc]);
escape_uri([Char | Rest], Acc) ->
    escape_uri(Rest, [io_lib:format("%~2.16.0B", [Char]) | Acc]).

-spec process_response(data(), #state{}, #torrent{}, #torrent_stats{}) -> 
 {#state{}, #torrent{}, #torrent_stats{}}.
process_response(RespDict, State, Torrent, TorrentStats) ->
    lists:foldl(fun process_response/2, 
     {State, Torrent, TorrentStats}, RespDict).

-spec process_response({binary, data()}, 
 {#state{}, #torrent{}, #torrent_stats{}}) -> 
 {#state{}, #torrent{}, #torrent_stats{}}.
process_response({<<"failure reason">>, Reason}, 
 {State, Torrent, TorrentStats}) ->
    io:format("Failure:~s~n", [binary_to_list(Reason)]),
    {State, Torrent, TorrentStats};
process_response({<<"warning message">>, Message},
 {State, Torrent, TorrentStats}) ->
    io:format("Warning:~s~n", [binary_to_list(Message)]),
    {State, Torrent, TorrentStats};
process_response({<<"interval">>, Interval},
 {State, Torrent, TorrentStats}) ->
    {State#state{interval= Interval}, Torrent, TorrentStats};
process_response({<<"min interval">>, MinInterval},
 {State, Torrent, TorrentStats}) ->
    {State#state{min_interval= MinInterval}, Torrent, TorrentStats};
process_response({<<"tracker id">>, TrackerId},
 {State, Torrent, TorrentStats}) ->
    {State, Torrent#torrent{trackerid= TrackerId}, TorrentStats};
process_response({<<"peers">>, PeerList},
 {State, Torrent, TorrentStats}) ->
    Peers = parse_peers(PeerList, []),
    {State#state{peers= Peers}, Torrent, TorrentStats};
process_response({<<"complete">>, Complete},
 {State, Torrent, TorrentStats = #torrent_stats{state= DState}}) 
        when DState =:= start ->
    {State#state{complete= Complete}, Torrent, 
     TorrentStats#torrent_stats{state= progressing}};
process_response({<<"complete">>, Complete},
 {State, Torrent, TorrentStats}) ->
    {State#state{complete= Complete}, Torrent, TorrentStats};
process_response({<<"incomplete">>, Incomplete},
 {State, Torrent, TorrentStats}) ->
    {State#state{incomplete= Incomplete}, Torrent, TorrentStats};
process_response({_Key, _Value},
 {State, Torrent, TorrentStats}) ->
    {State, Torrent, TorrentStats}.

-spec parse_peers(binary(), list()) -> [{string(), non_neg_integer()}].
parse_peers(<<>>, Acc) ->
    Acc;
parse_peers(<<IP1:8, IP2:8, IP3:8, IP4:8, Port:16, Rest/binary>>, Acc) ->
    parse_peers(Rest, [{lists:flatten(
        io_lib:format("~p.~p.~p.~p", [IP1, IP2, IP3, IP4])), Port} | Acc]).

%%TODO: Should min interval be taken into account here?    
start_timer(State = #state{interval= Interval, min_interval= _MinInterval}) ->
    Tref = erlang:start_timer(Interval * 1000, self(), announce),
    State#state{timer= Tref}.

