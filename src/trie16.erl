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

-export([map_set/3, map_get/2]).

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

all_keys(A) -> [X || X<-lists:seq(0,255), map_get(X, A) =/= []].

map_test_() ->
  [?_assertEqual(" !HWdelor",
      all_keys(lists:foldl(fun(X, D) -> map_set(X, X, D) end, [], "Hello World!")))].
