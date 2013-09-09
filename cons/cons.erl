%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/cons/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% Sample data set
%% >Rosalind_1
%% ATCCAGCT
%% >Rosalind_2
%% GGGCAACT
%% >Rosalind_3
%% ATGGATCT
%% >Rosalind_4
%% AAGCAACC
%% >Rosalind_5
%% TTGGAACT
%% >Rosalind_6
%% ATGCCATT
%% >Rosalind_7
%% ATGGCACT

%% Sample output
%% ATGCAACT
%% A: 5 1 0 0 5 5 0 0
%% C: 0 0 1 4 2 0 6 1
%% G: 1 1 6 3 0 1 0 0
%% T: 1 5 0 0 0 1 1 6


-module(cons).
-include_lib("eunit/include/eunit.hrl").
-export([file/1, run/1, o/1]).

%% Use first key returned by fetch_keys, grab that value, and size it
fafsa_length(Dict) ->
    length(dict:fetch(hd(dict:fetch_keys(Dict)), Dict)).

update_matrix([], Matrix, _Index) ->
    Matrix;
update_matrix([H|T], Matrix, Index) ->
    OldRow = proplists:get_value(H, Matrix),
    NewTuple = {H, setelement(Index, OldRow,
                              element(Index, OldRow) + 1) },
    update_matrix(T, lists:keyreplace(H, 1, Matrix, NewTuple),
                  Index + 1).
             
%% dict:fold
count_symbols(_Key, Seq, Matrix) ->
    update_matrix(Seq, Matrix, 1).

find_profile_matrix(Dict) ->
    Length = fafsa_length(Dict),
    Matrix = [ { $A, erlang:make_tuple(Length, 0) },
               { $C, erlang:make_tuple(Length, 0) },
               { $G, erlang:make_tuple(Length, 0) },
               { $T, erlang:make_tuple(Length, 0) } ],
    {Length, dict:fold(fun count_symbols/3, Matrix, Dict)}.

consensus_value(0, _A, _C, _G, _T, Accum) ->
    Accum;
consensus_value(Index, A, C, G, T, Accum) ->
    consensus_value(Index - 1,
                    A, C, G, T,
                    [highest(element(Index, A),
                            element(Index, C),
                            element(Index, G),
                            element(Index, T))|Accum]).

highest(A, C, G, T) when A >= C, A >= G, A >= T ->
    $A;
highest(_A, C, G, T) when C >= G, C >= T ->
    $C;
highest(_A, _C, G, T) when G >= T ->
    $G;
highest(_A, _C, _G, _T) ->
    $T.


find_consensus_string(Len, Matrix) ->
    consensus_value(Len,
                    proplists:get_value($A, Matrix),
                    proplists:get_value($C, Matrix),
                    proplists:get_value($G, Matrix),
                    proplists:get_value($T, Matrix),
                    []).
                    
    

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
    {Len, Matrix} = find_profile_matrix(process_lines(split_lines(Seq))),
    Cons = find_consensus_string(Len, Matrix),
    { Cons, Matrix }.

sample_cons_test() ->
    {Cons, Matrix} = run(process_lines(test_long_fasta())),
    ?assertEqual("ATGCAACT", Cons),
    ?assertEqual(proplists:get_value("C", Matrix),
                 [0, 0, 1, 4, 2, 0, 6, 1]),
    ok.

%% The file sample.cons has this, but here also:
test_long_fasta() ->
    [
     ">Rosalind_1",
     "ATCCAGCT",
     ">Rosalind_2",
     "GGGCAACT",
     ">Rosalind_3",
     "ATGGATCT",
     ">Rosalind_4",
     "AAGCAACC",
     ">Rosalind_5",
     "TTGGAACT",
     ">Rosalind_6",
     "ATGCCATT",
     ">Rosalind_7",
     "ATGGCACT"
    ].
    

o(Char, List) ->
    io:format("~c: ~s~n", [Char,
                           string:join(lists:map(fun(X) -> integer_to_list(X) end,
                                                 tuple_to_list(List)),
                                       " ")]).

o({Cons, Matrix}) ->
    io:format("~s~n", [Cons]),
    o($A, proplists:get_value($A, Matrix)),
    o($C, proplists:get_value($C, Matrix)),
    o($G, proplists:get_value($G, Matrix)),
    o($T, proplists:get_value($T, Matrix)).
    
