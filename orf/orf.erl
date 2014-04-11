%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/orf/
%%% @end
%%% Created :  10 Apr 2014 by John Daily <jd@epep.us>

%% Sample input:
%% >Rosalind_99
%% AGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG
%% Sample output:
%% MLLGSFRLIPKETLIQVAGSSPCNLS
%% M
%% MGMTPRLGLESLLE
%% MTPRLGLESLLE

-module(orf).
-include_lib("eunit/include/eunit.hrl").
-export([run/1, file/1, o/1, sample_run/0]).


%% Fasta handling, should create a new module for all this

parse_fasta(Lines) ->
    parse_fasta(Lines, [], []).

%% We're done
parse_fasta([], [], Accum) ->
    dict:from_list(Accum);
parse_fasta([], Block, Accum) ->
    parse_fasta([], [], process_block(Block, Accum));
%% Skip blank lines
parse_fasta([[]|T], Block, Accum) ->
    parse_fasta(T, Block, Accum);
%% If we see > at the beginning of a line, new block
parse_fasta([[SH|ST]|T], Block, Accum) when SH =:= $> ->
    parse_fasta(T, [ST], process_block(Block, Accum));
parse_fasta([H|T], Block, Accum) ->
    parse_fasta(T, [H|Block], Accum).

process_block([], Rest) ->
    Rest;
process_block(Block, Rest) ->
    NewBlock = lists:reverse(Block),
    [{ hd(NewBlock), lists:flatten(lists:nthtail(1, NewBlock)) } | Rest].

split_lines(Seq) ->
    re:split(Seq, "[\\n\\r]+", [{return, list}]).

%% -spec map_fasta_values(dict(), fun((string()) -> term())).
map_fasta_values(Dict, Fun) ->
    dict:fold(fun(_Key, Value, Acc) -> Acc ++ [Fun(Value)] end, [], Dict).

%% End of Fasta handling

%% DNA -> RNA

dna2rna(DNA) ->
    dna2rna(DNA, []).

dna2rna([], New) ->
    lists:reverse(New);
dna2rna([H|T], New) when H =:= $T ->
    dna2rna(T, [$U|New]);
dna2rna([H|T], New) ->
    dna2rna(T, [H|New]).

%% End of DNA -> RNA

file(Filename) ->
    {ok, FH} = file:open(Filename, [read]),
    {ok, Contents} = file:read(FH, 10000),
    file:close(FH),
    run(Contents).

run(Seq) ->
    RNA = hd(map_fasta_values(parse_fasta(split_lines(Seq)),
                              fun dna2rna/1)),
    find_orfs(RNA, []).

find_orfs([], Accum) ->
    Accum;
%% Start codon is AUG
find_orfs([$A, $U, $G|_Tail]=Seq, Accum) ->
    {Protein, Rest} = rna2prot(Seq, [], mapping()),
    find_orfs(Rest, Accum ++ [Protein]);
find_orfs([_H|T], Accum) ->
    find_orfs(T, Accum).


rna2prot([], New, _Map) ->
    {lists:reverse(lists:flatten(New)), []};
rna2prot([A,B,C|T], New, Map) ->
    do_map(T, dict:find([A, B, C], Map), New, Map).

do_map(Next, {ok, stop}, Accum, _Map) ->
    {lists:reverse(lists:flatten(Accum)), Next};
do_map(Next, {ok, Mapping}, Accum, Map) ->
    rna2prot(Next, [Mapping|Accum], Map);
do_map(_Next, error, _Accum, _Map) ->
    throw(no_such_sequence).

o([]) ->
    ok;
o([Seq|Tail]) ->
    io:format("~s~n", [Seq]),
    o(Tail).

sample_run() ->
    run(">Rosalind_99\nAGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG").

orf_test_() ->
    [?_assert(lists:sort(run(">Rosalind_99\nAGCCATGTAGCTAACTCAGGTTACATGGGGATGACCCCGCGACTTGGATTAGAGTCTCTTTTGGAATAAGCCTGAATGATCCGAGTAGCATCTCAG")) =:=
                             ["M",
                              "MGMTPRLGLESLLE",
                              "MLLGSFRLIPKETLIQVAGSSPCNLS",
                              "MTPRLGLESLLE"])
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
