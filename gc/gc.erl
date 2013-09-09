%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/gc/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample data set
%% >Rosalind_6404
%% CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC
%% TCCCACTAATAATTCTGAGG
%% >Rosalind_5959
%% CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT
%% ATATCCATTTGTCAGCAGACACGC
%% >Rosalind_0808
%% CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC
%% TGGGAACCTGCGGGCAGTAGGTGGAAT

%% Sample output
%% Rosalind_0808
%% 60.919540

-module(gc).
-include_lib("eunit/include/eunit.hrl").
-export([file/1, run/1, o/1]).

file(Filename) ->
    {ok, FH} = file:open(Filename, [read]),
    {ok, Contents} = file:read(FH, 10000),
    file:close(FH),
    run(Contents).

split_lines(Seq) ->
    re:split(Seq, "[\\n\\r]+", [{return, list}]).

process_lines(Lines) ->
    process_lines(Lines, [], []).

%% We're done
process_lines([], [], Accum) ->
    dict:from_list(Accum);
process_lines([], Block, Accum) ->
    process_lines([], [], process_block(Block, Accum));
%% Skip blank lines
process_lines([[]|T], Block, Accum) ->
    process_lines(T, Block, Accum);
%% If we see > at the beginning of a line, new block
process_lines([[SH|ST]|T], Block, Accum) when SH =:= $> ->
    process_lines(T, [ST], process_block(Block, Accum));
process_lines([H|T], Block, Accum) ->
    process_lines(T, [H|Block], Accum).

process_block([], Rest) ->
    Rest;
process_block(Block, Rest) ->
    NewBlock = lists:reverse(Block),
    [{ hd(NewBlock), lists:flatten(lists:nthtail(1, NewBlock)) } | Rest].

run(Seq) ->
    find_highest_gc(process_lines(split_lines(Seq))).

gc(Seq) ->
    {GC, Total} =
        lists:foldl(fun(Char, {GC, Total}) when Char =:= $G orelse Char =:= $C ->
                            {GC + 1, Total + 1};
                       (_Char, {GC, Total}) ->
                            {GC, Total + 1}
                    end,
                    {0, 0}, Seq),
    GC / Total.

find_highest_gc(Dict) ->
    dict:fold(fun(Key, Value, {HighKey, HighGC}) ->
                      GC = gc(Value),
                      if GC > HighGC ->
                              {Key, GC};
                         true ->
                              {HighKey, HighGC}
                      end
              end,
              {"", 0},
              Dict).

highest_gc_test() ->
    {Key, GC} = find_highest_gc(dict:from_list([{"A", "ABC"}, {"B", "CBCDEFG"}])),
    ?assertEqual("B", Key),
    ?assert(abs(GC - 0.42857142857142855) < 0.001),
    ok.

sample_gc_test() ->
    {Key, GC} = find_highest_gc(process_lines(test_long_fasta())),
    ?assertEqual("Rosalind_0808", Key),
    ?assert(abs(GC - 0.60919540) < 0.001),
    ok.

gc_test_() ->
     [?_assert(abs(gc("GTCT") - 0.5) < 0.001),
      ?_assert(abs(gc("GTT") - (1/3)) < 0.001),
      ?_assert(split_lines(io_lib:format("A~nB~nC~n", [])) =:= ["A", "B", "C", ""])
     ].


%% The file sample.gc has this, but here also:
test_long_fasta() ->
    [
     ">Rosalind_6404",
     "CCTGCGGAAGATCGGCACTAGAATAGCCAGAACCGTTTCTCTGAGGCTTCCGGCCTTCCC",
     "TCCCACTAATAATTCTGAGG",
     ">Rosalind_5959",
     "CCATCGGTAGCGCATCCTTAGTCCAATTAAGTCCCTATCCAGGCGCTCCGCCGAAGGTCT",
     "ATATCCATTTGTCAGCAGACACGC",
     ">Rosalind_0808",
     "CCACCCTCGTGGTATGGCTAGGCATTCAGGAACCGGAGAACGCTTCAGACCAGCCCGGAC",
     "TGGGAACCTGCGGGCAGTAGGTGGAAT"
    ].
    

o({ID, GC}) ->
    io:format("~s~n~.6f~n", [ID, GC * 100]).
