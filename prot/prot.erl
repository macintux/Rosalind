%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/prot/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample input: AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA
%% Sample output: MAMAPRTEINSTRING

-module(prot).
-include_lib("eunit/include/eunit.hrl").
-export([run/1, file/1, o/1]).

file(Filename) ->
    {ok, FH} = file:open(Filename, [read]),
    {ok, Contents} = file:read(FH, 10000),
    file:close(FH),
    run(Contents).

run(Seq) ->
    run(Seq, [], mapping()).

run([], New, _Map) ->
    lists:reverse(lists:flatten(New));
run([A,B,C|T], New, Map) ->
    do_map(T, dict:find([A, B, C], Map), New, Map).

do_map(_Next, {ok, stop}, Accum, _Map) ->
    lists:reverse(lists:flatten(Accum));
do_map(Next, {ok, Mapping}, Accum, Map) ->
    run(Next, [Mapping|Accum], Map);
do_map(_Next, error, _Accum, _Map) ->
    throw(no_such_sequence).

o(Seq) ->
    io:format("~s~n", [Seq]).

prot_test_() ->
    [?_assert(run("AUGGCCAUGGCGCCCAGAACUGAGAUCAAUAGUACCCGUAUUAACGGGUGA") =:= "MAMAPRTEINSTRING"),
     ?_assert(run("CUCUUU") =:= "LF")
    ].

mapping() ->
    dict:from_list([{"UUU", "F"},
                    {"CUU", "L"},
                    {"AUU", "I"},
                    {"GUU", "V"},
                    {"UUC", "F"},
                    {"CUC", "L"},
                    {"AUC", "I"},
                    {"GUC", "V"},
                    {"UUA", "L"},
                    {"CUA", "L"},
                    {"AUA", "I"},
                    {"GUA", "V"},
                    {"UUG", "L"},
                    {"CUG", "L"},
                    {"AUG", "M"},
                    {"GUG", "V"},
                    {"UCU", "S"},
                    {"CCU", "P"},
                    {"ACU", "T"},
                    {"GCU", "A"},
                    {"UCC", "S"},
                    {"CCC", "P"},
                    {"ACC", "T"},
                    {"GCC", "A"},
                    {"UCA", "S"},
                    {"CCA", "P"},
                    {"ACA", "T"},
                    {"GCA", "A"},
                    {"UCG", "S"},
                    {"CCG", "P"},
                    {"ACG", "T"},
                    {"GCG", "A"},
                    {"UAU", "Y"},
                    {"CAU", "H"},
                    {"AAU", "N"},
                    {"GAU", "D"},
                    {"UAC", "Y"},
                    {"CAC", "H"},
                    {"AAC", "N"},
                    {"GAC", "D"},
                    {"UAA", stop},
                    {"CAA", "Q"},
                    {"AAA", "K"},
                    {"GAA", "E"},
                    {"UAG", stop},
                    {"CAG", "Q"},
                    {"AAG", "K"},
                    {"GAG", "E"},
                    {"UGU", "C"},
                    {"CGU", "R"},
                    {"AGU", "S"},
                    {"GGU", "G"},
                    {"UGC", "C"},
                    {"CGC", "R"},
                    {"AGC", "S"},
                    {"GGC", "G"},
                    {"UGA", stop},
                    {"CGA", "R"},
                    {"AGA", "R"},
                    {"GGA", "G"},
                    {"UGG", "W"},
                    {"CGG", "R"},
                    {"AGG", "R"},
                    {"GGG", "G"}]).
