%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/fib/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: 5 3
%% Sample output: 19
%% Fibonacci with a twist.

-module(fib).
-include_lib("eunit/include/eunit.hrl").
-export([run/2, o/1]).

run(1, _K) ->
    1;
run(2, _K) ->
    1;
run(N, K) ->
    %% Every pair of rabbits at least 2 months old generates K pairs
    %% this time to add to however many pairs we had last month
    (run(N-2, K)*K) + run(N-1, K).

o(N) ->
    io:format("~B~n", [N]).

%% I can't find any resources on the web which match this problem
%% exactly so it's tough to get alternate test cases.
fib_test_() ->
    [?_assert(run(1, 3) =:= 1),
     ?_assert(run(5, 3) =:= 19)
     ].
