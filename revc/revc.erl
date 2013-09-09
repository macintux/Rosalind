%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/revc/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: AAAACCCGGT
%% Sample output: ACCGGGTTTT
%% Reverse list, then A swaps with T and G swaps with C

-module(revc).
-include_lib("eunit/include/eunit.hrl").
-export([run/1, o/1]).

run(Seq) ->
    run(Seq, []).

run([], New) ->
    New; %% No need to reverse, that's one of the criteria for the problem
run([H|T], New) when H =:= $T ->
    run(T, [$A|New]);
run([H|T], New) when H =:= $A ->
    run(T, [$T|New]);
run([H|T], New) when H =:= $C ->
    run(T, [$G|New]);
run([H|T], New) when H =:= $G ->
    run(T, [$C|New]).

o(Seq) ->
    io:format("~s~n", [Seq]).

rna_test_() ->
    [?_assert(run("A") =:= "T"),
     ?_assert(run("C") =:= "G"),
     ?_assert(run("AAAACCCGGT") =:= "ACCGGGTTTT")
    ].

