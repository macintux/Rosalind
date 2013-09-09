%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/subs/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

-module(subs).
-include_lib("eunit/include/eunit.hrl").
-export([run/2, o/1]).

run(Seq, Subseq) ->
    run(Seq, Subseq, 1, []).

run([], _Subseq, _Index, Matches) ->
    lists:reverse(Matches);
run([_H1|T1]=Seq, [_H1|T2]=Subseq, Index, Matches) ->
    run(T1, Subseq, Index + 1,
        check_match(string:substr(Seq, 1, length(Subseq)),
                    Subseq, Index, Matches));
run([H1|T1]=Seq, [H2|T2]=Subseq, Index, Matches) ->
    run(T1, Subseq, Index + 1, Matches).

check_match(_Seq, _Seq, Index, Matches) ->
    [Index|Matches];
check_match(_, _, _Index, Matches) ->
    Matches.


subs_test_() ->
    [?_assert(run("GATATATGCATATACTT", "ATAT") =:= [2, 4, 10])
    ].

o(List) ->
    io:format("~s~n", [string:join(lists:map(fun(X) -> integer_to_list(X) end,
                                            List), " ")]).
