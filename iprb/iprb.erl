%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%  http://rosalind.info/problems/iprb/
%%% @end
%%% Created :  8 Sep 2013 by John Daily <jd@epep.us>

%% There are at least 3 solutions here:
%%   1 Build a probability tree
%%   2 Create a bag of organisms and evaluate all the possible combinations
%%   3 Break down the probability tree into a brute force equation with all possible paths

%% #3 feels like cheating, but it's also by far the most
%% straightforward approach and absolutely the one I'd use in a
%% production environment.

%% If we assume T = (k + m + n), the paths are:
%%    k/T * (k-1)/(T-1) * 1
%%    k/T * m/(T-1) * 1
%%    k/T * n/(T-1) * 1
%%    m/T * k/(T-1) * 1
%%    m/T * (m-1)/(T-1) * .75
%%    m/T * n/(T-1) * .5
%%    n/T * k/(T-1) * 1
%%    n/T * m/(T-1) * .5
%%    And the last path includes a factor of 0, so it can be ignored
%% Sum all of the above and you have your answer.

-module(iprb).
-include_lib("eunit/include/eunit.hrl").
-export([run/3, o/1]).

run(K, M, N) ->
    T = K + M + N,
    Denom = T*T - T,
    sum([
         (K*K - K) / Denom,
         K*M / Denom,
         K*N / Denom,
         M*K / Denom,
         (M*M - M) * 0.75 / Denom,
         M*N * 0.5 / Denom,
         N*K / Denom,
         N*M * 0.5 / Denom]).

sum(List) ->
    lists:foldl(fun(X, Accum) -> X + Accum end,
                0, List).

iprb_test_() ->
    [?_assert(abs(run(2, 2, 2) - 0.78333) < 0.001)
    ].

o(Prob) ->
    io:format("~.5f~n", [Prob]).

