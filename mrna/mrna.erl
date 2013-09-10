%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/mrna/
%%% @end
%%% Created :  9 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: MA
%% Sample output: 12

-module(mrna).
-include_lib("eunit/include/eunit.hrl").
-export([run/1, o/1]).

run(Seq) ->
    lists:foldl(fun(X, Accum) ->
                        dict:fetch(X, mapping()) * Accum
                end,
                1,
                Seq) * dict:fetch(stop, mapping()).
                         
mrna_test_() ->
    [?_assert(12 =:= run("MA"))
    ].

o(Val) ->
    io:format("~B~n", [Val]).

mapping() ->
    dict:from_list([{$F, 2},
                    {$L, 6},
                    {$I, 3},
                    {$V, 4},
                    {$M, 1},
                    {$S, 6},
                    {$P, 4},
                    {$T, 4},
                    {$A, 4},
                    {$Y, 2},
                    {$H, 2},
                    {$N, 2},
                    {$D, 2},
                    {$Q, 2},
                    {$K, 2},
                    {$E, 2},
                    {$C, 2},
                    {$R, 6},
                    {$G, 4},
                    {$W, 1},
                    {stop, 3}]).
