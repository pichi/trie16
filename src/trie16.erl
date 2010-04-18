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

-export([set/3, get/2]).

set(I, V, A) ->
    H = I bsr 4,
    L = I band 15,
    LS = get_from_segment(H, A),
    LS2 = set_to_segment(L, V, LS),
    set_to_segment(H, LS2, A).

get(_, []) -> [];
get(I, A) ->
    H = I bsr 4,
    L = I band 15,
    LS = get_from_segment(H, A),
    get_from_segment(L, LS).

set_to_segment(0, V, []) ->
    {V, [], [], [], [], [], [], [], [], [], [], [], [], [], [], []};
set_to_segment(1, V, []) ->
    {[], V, [], [], [], [], [], [], [], [], [], [], [], [], [], []};
set_to_segment(2, V, []) ->
    {[], [], V, [], [], [], [], [], [], [], [], [], [], [], [], []};
set_to_segment(3, V, []) ->
    {[], [], [], V, [], [], [], [], [], [], [], [], [], [], [], []};
set_to_segment(4, V, []) ->
    {[], [], [], [], V, [], [], [], [], [], [], [], [], [], [], []};
set_to_segment(5, V, []) ->
    {[], [], [], [], [], V, [], [], [], [], [], [], [], [], [], []};
set_to_segment(6, V, []) ->
    {[], [], [], [], [], [], V, [], [], [], [], [], [], [], [], []};
set_to_segment(7, V, []) ->
    {[], [], [], [], [], [], [], V, [], [], [], [], [], [], [], []};
set_to_segment(8, V, []) ->
    {[], [], [], [], [], [], [], [], V, [], [], [], [], [], [], []};
set_to_segment(9, V, []) ->
    {[], [], [], [], [], [], [], [], [], V, [], [], [], [], [], []};
set_to_segment(10, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], V, [], [], [], [], []};
set_to_segment(11, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], V, [], [], [], []};
set_to_segment(12, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], V, [], [], []};
set_to_segment(13, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], V, [], []};
set_to_segment(14, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], V, []};
set_to_segment(15, V, []) ->
    {[], [], [], [], [], [], [], [], [], [], [], [], [], [], [], V};
set_to_segment(I, V, S) ->
    setelement(I+1, S, V).

get_from_segment(_, []) -> [];
get_from_segment(I, S) -> element(I+1, S).

all_keys(A) -> [X || X<-lists:seq(0,255), get(X, A) =/= []].

map_test_() ->
  [?_assertEqual(" !HWdelor",
      all_keys(lists:foldl(fun(X, D) -> set(X, X, D) end, [], "Hello World!")))].
