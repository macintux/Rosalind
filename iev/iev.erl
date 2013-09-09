%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/iev/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

-module(iev).
-include_lib("eunit/include/eunit.hrl").
-export([run/6, o/1]).

%% Couples:
%%   C1 : AA - AA -> 2 children with dominant
%%   C2 : AA - Aa -> 2
%%   C3 : AA - aa -> 2
%%   C4 : Aa - Aa -> 1.5
%%   C5 : Aa - aa -> 1
%%   C6 : aa - aa -> 0
run(C1, C2, C3, C4, C5, _C6) ->
    sum([
         C1 * 2,
         C2 * 2,
         C3 * 2,
         C4 * 1.5,
         C5]).

sum(List) ->
    lists:foldl(fun(X, Accum) -> X + Accum end,
                0, List).

iev_test_() ->
    [?_assert(abs(run(1, 0, 0, 1, 0, 1) - 3.5) < 0.001)
    ].

o(Prob) ->
    io:format("~.5f~n", [Prob]).

