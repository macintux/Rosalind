%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%   http://rosalind.info/problems/dna/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC
%% Sample output: 20 12 17 21
%% 20 As, 12 Cs, 17 Gs, 21 Ts

-module(dna).
-export([run/1, output/1]).
-include_lib("eunit/include/eunit.hrl").

run(Seq) ->
    run(Seq, {0, 0, 0, 0}).

run([], Tally) ->
    Tally;
run([H|T], Tally) when H == $A ->
    run(T, setelement(1, Tally, element(1, Tally) + 1));
run([H|T], Tally) when H == $C ->
    run(T, setelement(2, Tally, element(2, Tally) + 1));
run([H|T], Tally) when H == $G ->
    run(T, setelement(3, Tally, element(3, Tally) + 1));
run([H|T], Tally) when H == $T ->
    run(T, setelement(4, Tally, element(4, Tally) + 1)).

output({A, C, G, T}) ->
    io:format("~B ~B ~B ~B~n", [A, C, G, T]).


dna_test_() ->
    [?_assert(run("A") =:= {1, 0, 0, 0}),
     ?_assert(run("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") =/= {20, 12, 17, 23}),
     ?_assert(run("AGCTTTTCATTCTGACTGCAACGGGCAATATGTCTCTGTGTGGATTAAAAAAAGAGTGTCTGATAGCAGC") =:= {20, 12, 17, 21}),
     ?_assertException(error, function_clause, run("B"))
    ].
