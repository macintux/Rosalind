%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/hamm/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample data set
%% GAGCCTACTAACGGGAT
%% CATCGTAATGACGGCCT
%% Sample output
%% 7

-module(hamm).
-include_lib("eunit/include/eunit.hrl").
-export([run/2, o/1]).

run(Seq1, Seq2) ->
    run(Seq1, Seq2, 0).

run([], [], Hamm) ->
    Hamm;
run([_H1|T1], [_H1|T2], Hamm) ->
    run(T1, T2, Hamm);
run([_H1|T1], [_H2|T2], Hamm) ->
    run(T1, T2, Hamm + 1).

hamm_test_() ->
    [?_assert(run("GAGC", "GAAC") =:= 1),
     ?_assert(run("GAGC", "GAGC") =:= 0),
     ?_assert(run("GAGC", "AGAA") =:= 4)
    ].

o(Hamm) ->
    io:format("~B~n", [Hamm]).
