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
-module(bencode_tests).

-export([test_no/2]).

-include_lib("eunit/include/eunit.hrl").

test_no(N, Tests) ->
	setelement(2, Tests,
        setelement(4, element(2, Tests),
            lists:nth(N, element(4, element(2, Tests))))).

%%% Eunit setup stuff

start_app() ->
    ok = application:start(crypto),
    ok = application:start(ssl),
    ok = application:start(torrenterl).

stop_app(_) ->
    ok = application:stop(torrenterl),
    ok = application:stop(ssl),
    ok = application:stop(crypto).

decode_test_() ->
    {setup, fun start_app/0, fun stop_app/1, [
            ?_test(decode_string()),
            ?_test(decode_integer()),
            ?_test(decode_list()),
            ?_test(decode_dictionary())
        ]}.

encode_test_() ->
    {setup, fun start_app/0, fun stop_app/1, [
            ?_test(encode_string()),
            ?_test(encode_integer()),
            ?_test(encode_list()),
            ?_test(encode_dictionary()) 
        ]}.

mixed_encode_decode_test_() ->
    {setup, fun start_app/0, fun stop_app/1, [
            ?_test(encode_decode_string()),
            ?_test(encode_decode_integer()),
            ?_test(encode_decode_list()),
            ?_test(encode_decode_dictionary()) 
        ]}.

%%% Tests

decode_string() ->
    Res = bencode:decode(<<"4:alma">>),
    ?assertEqual(<<"alma">>, element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).

decode_integer() ->
    Res = bencode:decode(<<"i3e">>),
    ?assertEqual(3, element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).

decode_list() ->
    Res = bencode:decode(<<"l4:almai3ee">>),
    ?assertEqual([<<"alma">>, 3], element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).

decode_dictionary() ->
    Res = bencode:decode(<<"d4:almai3e4:bela4:teste">>),
    ?assertEqual([{<<"alma">>, 3}, {<<"bela">>, <<"test">>}],
     element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).

encode_string() ->
    Res = bencode:encode(<<"almafa">>),
    ?assertEqual(<<"6:almafa">>, Res).

encode_integer() ->
    Res = bencode:encode(3456),
    ?assertEqual(<<"i3456e">>, Res). 

encode_list() ->
    Res = bencode:encode([<<"alma">>, 3]),
    ?assertEqual(<<"l4:almai3ee">>, Res).

encode_dictionary() ->
    Res = bencode:encode([{<<"alma">>, 3}, {<<"bela">>, <<"test">>}]),
    ?assertEqual(<<"d4:almai3e4:bela4:teste">>, Res). 

encode_decode_string() ->
    String = <<"alma">>,
    Res = bencode:decode(bencode:encode(String)),
    ?assertEqual(String, element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).

encode_decode_integer() ->
    Integer = 345,
    Res = bencode:decode(bencode:encode(Integer)),
    ?assertEqual(Integer, element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)).


encode_decode_list() ->
    List = [<<"alma">>, 42, <<"testing">>],
    Res = bencode:decode(bencode:encode(List)),
    ?assertEqual(List, element(1, Res)),
    ?assertEqual(<<>>, element(2, Res)). 


encode_decode_dictionary() ->
    Dictionary = [{<<"bela">>, 42}, {<<"alma">>, <<"testing">>}, 
     {<<"A">>, [42, <<"cecil">>]}], 
    Res = bencode:decode(bencode:encode(Dictionary)),
    ?assertEqual(lists:keysort(1, Dictionary), element(1, Res)), 
    ?assertEqual(<<>>, element(2, Res)).

