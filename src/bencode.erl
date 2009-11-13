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
%%% @doc Module implementing bencode format encoding and decoding
%%% See {@link decode/1} and {@link encode/1} functions.
%%% @end
-module(bencode).

-export([decode/1, encode/1]).

-export([decode_file/1]).

-include("torrenterl_types.hrl").

%% @spec (Path) -> Data
%%   Path = string()
%%   Data = binary | integer() | list()
%% @doc Decodes bencoded file.
%% Reads up file content and calls {@link decode/1} on it.
%% @end
-spec decode_file(string()) -> data().
decode_file(File) ->
    {ok, Bin} = file:read_file(File),
    decode(Bin).

%% @spec (BencBin) -> Data 
%%   BencBin = binary()
%%   Data = binary() | integer() | list()  
%% @doc Decodes bencoded data.
%% Bencoding is a way to specify and organize data in a terse format. 
%% It supports the following types: byte strings, integers, lists, 
%% and dictionaries. Full definition of each type can be found on: 
%% `http://wiki.theory.org/BitTorrentSpecification'
%%
%% The bencoded data types are mapped to erlang data types the following way:
%%
%% BString -> String in binary format (`<<"4:alma">>' -> `<<"alma">>')
%%
%% BInteger -> Integer (`<<"i42e">>' -> `42' )
%%
%% BList -> List (`<<"l4:almai3e">>' -> `[<<"alma">>, 3]')
%%
%% BDictionary -> List of 2-tuples 
%%  (`<<"d4:almai42e3:foo3:bare">>' -> 
%%   `[{<<"alma">>, 42}, {<<"foo">>, <<"bar">>}]')
%% @end
-spec decode(b_data()) -> data().
decode(<<$d, Rest/binary>>) ->
    decode_dictionary(Rest, []);
decode(<<$l, Rest/binary>>) ->
    decode_list(Rest, []);
decode(<<$i, Rest/binary>>) ->
    decode_integer(Rest, []);
decode(BencodedBin) ->
    decode_string(BencodedBin). 

%% @spec (Data::Data) -> BencBin
%%   Data = binary() | integer() | list() 
%%   BencBin = binary() 
%% @doc Encodes data to bencoded format.
%% For more information on the bencoded format go to {@link decode/1}.
%%
%% Note: In case of Dictionaries (list of 2-tuples) the elements are reordered
%% to adhere to the specification. Which states that keys have to be ordered in
%% increasing order by comparing their binary representations.
%% @end
-spec encode(data()) -> b_data().
encode(String) when is_binary(String) ->
    Size  = list_to_binary(integer_to_list(size(String))), 
    <<Size/binary, ":", String/binary>>; 
encode(Integer) when is_integer(Integer) ->
    BinRep = list_to_binary(integer_to_list(Integer)),
    <<$i, BinRep/binary, $e>>;
encode([{_Key, _Value} | _Rest] = Dictionary) ->
    SortedDict = lists:keysort(1, Dictionary),
    BinRep = list_to_binary([[encode(Id), encode(Val)] || 
     {Id, Val} <- SortedDict]),
    <<$d, BinRep/binary, $e>>;
encode(List) when is_list(List) ->
    BinRep = list_to_binary([encode(Elem) || Elem <- List]),
    <<$l, BinRep/binary, $e>>.


%%% Internal functions
-spec decode_string(b_string()) -> string().
decode_string(Bin) ->
    {Length, Rest} = decode_string_length(Bin, []),
    decode_string(Rest, Length).

-spec decode_string_length(binary(), list()) -> {integer(), binary()}.
decode_string_length(<<":", Rest/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Rest};
decode_string_length(<<Char, Rest/binary>>, Acc) ->
    decode_string_length(Rest, [Char | Acc]).

-spec decode_string(binary(), integer()) -> {string(), integer()}.
decode_string(Bin, Length) ->
    <<String:Length/bytes, Rest/binary>> = Bin,
    {String, Rest}.

-spec decode_integer(binary(), list()) -> {integer(), binary()}.
decode_integer(<<$e, Rest/binary>>, Acc) ->
    {list_to_integer(lists:reverse(Acc)), Rest};
decode_integer(<<Element, Rest/binary>>, Acc) ->
    decode_integer(Rest, [Element | Acc]).

-spec decode_list(binary(), list()) -> {list(), binary()}.
decode_list(<<$e, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_list(Bin, Acc) ->
    {PartResult, Rest} = decode(Bin),
    decode_list(Rest, [PartResult | Acc]). 

-spec decode_dictionary(binary(), list()) -> {erdict(), binary()}.
decode_dictionary(<<$e, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
decode_dictionary(Bin, Acc) ->
    {Tag, Rest} = decode_string(Bin),
    {Value, NewRest} = decode(Rest),
    decode_dictionary(NewRest, [{Tag, Value} | Acc]).

