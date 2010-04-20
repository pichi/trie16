%%%-------------------------------------------------------------------
%%% File    : trie16.erl
%%% Authors : Hynek Vychodil <vychodil.hynek@gmail.com>
%%% License :
%%% Copyright 2010 Hynek Vychodil. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without modification, are
%%% permitted provided that the following conditions are met:
%%% 
%%%    1. Redistributions of source code must retain the above copyright notice, this list of
%%%       conditions and the following disclaimer.
%%% 
%%%    2. Redistributions in binary form must reproduce the above copyright notice, this list
%%%       of conditions and the following disclaimer in the documentation and/or other materials
%%%       provided with the distribution.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY HYNEK VYCHODIL ``AS IS'' AND ANY EXPRESS OR IMPLIED
%%% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND
%%% FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
%%% CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
%%% ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% 
%%% The views and conclusions contained in the software and documentation are those of the
%%% authors and should not be interpreted as representing official policies, either expressed
%%% or implied, of Hynek Vychodil.
%%%
%%% @doc 
%%% <p>trie16 is byte string trie using byte map stored in segments with 16 slots.</p>
%%%
%%% @end
%%%
%%% Created : 18 Apr 2010 by Hynek Vychodil <vychodil.hynek@gmail.com>
%%%-------------------------------------------------------------------
-module(trie16).

-include_lib("eunit/include/eunit.hrl").

-export([new/0, set/3, get/2]).

new() -> [].

set(K, V, T) when is_binary(K) ->
  set_(K, V, T).

% trie tree contain nodes
% {K, S, T} where K is bynary string, S is subtree, T value for key <<>> i.e. terminal
% {M, T} where M is map contain subtrees as values, T is terminal as above
% {[], T} as terminal node
% [] - empty i.e. unset node
set_(<<>>, V, []) -> term(V);
set_(<<>>, V, {M, _}) -> {M, V};
set_(<<>>, V, {K, S, _}) -> {K, S, V};
set_(K, V, []) -> {K, term(V), []};
set_(<<H:4, L:4, Rest/bytes>>, V, {M, V2}) ->
  ST = set_(Rest, V, map_get(H, L, M)),
  {map_set(H, L, ST, M), V2};
set_(K, V, {K, ST, V2}) -> {K, set_(<<>>, V, ST), V2};
set_(K, V, {K2, ST, V2}) ->
  case prefix(K, K2, <<>>) of
    {<<>>, K, K2} -> {splitmap(K, K2, V, ST), V2};
    {P, <<>>, R2} -> {P, {R2, ST, V}, V2};
    {P, R1, <<>>} -> {P, set_(R1, V, ST), V2};
    {P, R1, R2} -> {P, {splitmap(R1, R2, V, ST), []}, V2}
  end.

-compile({inline, [term/1]}).

term(V) -> {[], V}.

prefix(<<A:8, R1/bytes>>, <<A:8, R2/bytes>>, P) ->
  prefix(R1, R2, <<P/bytes, A:8>>);
prefix(R1, R2, P) -> {P, R1, R2}.

splitmap(<<H1:4, L1:4, R1/bytes>>, <<H2:4, L2:4, R2/bytes>>, V, ST) ->
  M = map_set(H1, L1, set_(R1, V, []), []),
  map_set(H2, L2, set2_(R2, ST), M).

-compile({inline, [set2_/2]}).

set2_(<<>>, ST) -> ST;
set2_(K, ST) -> {K, ST, []}.

get(K, T) when is_binary(K) ->
  get_(K, T).

get_(<<>>, {_, V}) -> V;
get_(<<>>, {_, _, V}) -> V;
get_(K, {K2, ST, _}) ->
  S2 = byte_size(K2),
  case K of
    <<K2:S2/bytes, R/bytes>> -> get_(R, ST);
    _ -> []
  end;
get_(<<H:4, L:4, R/bytes>>, {M, _}) -> get_(R, map_get(H, L, M));
get_(_, []) -> [].

map_set(I, V, A) ->
    map_set(I bsr 4, I band 15, V, A).

map_set(H, L, V, A) ->
    LS = segment_get(H, A),
    LS2 = segment_set(L, V, LS),
    segment_set(H, LS2, A).

map_get(_, []) -> [];
map_get(I, A) ->
    map_get(I bsr 4, I band 15, A).

map_get(_, _, []) -> [];
map_get(H, L, A) ->
    LS = segment_get(H, A),
    segment_get(L, LS).

segment_set(0, V, []) ->
    {V, [], [], [], [], [], [], [], [], [], [], [], [], [], [], []};
segment_set(1, V, []) ->
    {[], V, [], [], [], [], [], [], [], [], [], [], [], [], [], []};
segment_set(2, V, []) ->
    {[], [], V, [], [], [], [], [], [], [], [], [], [], [], [], []};
segment_set(3, V, []) ->
    {[], [], [], V, [], [], [], [], [], [], [], [], [], [], [], []};
segment_set(4, V, []) ->
    {[], [], [], [], V, [], [], [], [], [], [], [], [], [], [], []};
segment_set(5, V, []) ->
    {[], [], [], [], [], V, [], [], [], [], [], [], [], [], [], []};
segment_set(6, V, []) ->
    {[], [], [], [], [], [], V, [], [], [], [], [], [], [], [], []};
segment_set(7, V, []) ->
    {[], [], [], [], [], [], [], V, [], [], [], [], [], [], [], []};
segment_set(8, V, []) ->
    {[], [], [], [], [], [], [], [], V, [], [], [], [], [], [], []};
segment_set(9, V, []) ->
    {[], [], [], [], [], [], [], [], [], V, [], [], [], [], [], []};
segment_set(10, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], V, [], [], [], [], []};
segment_set(11, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], V, [], [], [], []};
segment_set(12, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], V, [], [], []};
segment_set(13, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], V, [], []};
segment_set(14, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], V, []};
segment_set(15, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], V};
segment_set(I, V, S) ->
    setelement(I+1, S, V).

segment_get(_, []) -> [];
segment_get(I, S) -> element(I+1, S).

map_keys(A) -> [X || X<-lists:seq(0,255), map_get(X, A) =/= []].

segment_test_() ->
  S = segment_set(3, a, segment_set(5, b, [])),
  [ ?_assertEqual(a, segment_get(3, S)),
    ?_assertEqual(b, segment_get(5, S))
    | [ ?_assertEqual([], segment_get(X, S))
      || X <- [0, 1, 2, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15]]].

map_test_() ->
  [ ?_assertEqual(" !HWdelor",
      map_keys(lists:foldl(
          fun(X, D) -> map_set(X, X, D) end,
          [], "Hello World!")))].

trie_test_() ->
  ?_assertEqual(bar, get(<<"foo">>, set(<<"foo">>, bar, new()))).
