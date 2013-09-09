%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/rna/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: GATGGAACTTGACTACGTAAATT
%% Sample output: GAUGGAACUUGACUACGUAAAUU

-module(rna).
-include_lib("eunit/include/eunit.hrl").
-export([run/1, o/1]).

run(Seq) ->
    run(Seq, []).

run([], New) ->
    lists:reverse(New);
run([H|T], New) when H =:= $T ->
    run(T, [$U|New]);
run([H|T], New) ->
    run(T, [H|New]).

o(Seq) ->
    io:format("~s~n", [Seq]).

rna_test_() ->
    [?_assert(run("A") =:= "A"),
     ?_assert(run("T") =:= "U"),
     ?_assert(run("GATGGAACTTGACTACGTAAATT") =:= "GAUGGAACUUGACUACGUAAAUU")
    ].

