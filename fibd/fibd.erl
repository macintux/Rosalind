%%% @author John Daily <jd@epep.us>
%%% @copyright (C) 2013, John Daily
%%% @doc
%%%   http://rosalind.info/problems/fibd/
%%% @end
%%% Created :  9 Sep 2013 by John Daily <jd@epep.us>

%% Mortal rabbits. I'm sure there's a top-down solution, but I'm
%% giving up and going for bottom up.
-module(fibd).
-include_lib("eunit/include/eunit.hrl").
-export([run/2, o/1]).

run(N, M) ->
    run(2, N+1, M, make_counters(M)).

run(_Max, _Max, _M, Counters) ->
    tally(Counters);
run(Current, Max, M, Counters) ->
    run(Current + 1, Max, M,
        update_counters(M, Counters)).

tally(List) ->
    lists:foldl(fun(X, Accum) -> Accum + X end, 0, List).

%% Each generation needs to:
%% 1 Tally the members of the counting list except for the first
%%   ("maturing rabbits") slot
%% 2 Drop the last element of the counting list; these are the rabbits
%%   who perish
%% 3 Place the tally from step 1 into the new first slot
update_counters(M, [_H|T]=Counters) ->
    NewChildren = tally(T),
    SurvivingRabbits = lists:sublist(Counters, M-1),
    [NewChildren|SurvivingRabbits].

%% Handy little trick for generating dynamic list of zeros from
%% http://stackoverflow.com/a/5989309/1527772
make_counters(M) ->
    [1|[0 || _X <- lists:seq(1, M-1)]].

fibd_test_() ->
    [?_assert(4 =:= run(6, 3)),
     ?_assert(7 =:= run(8, 3))
     ].


o(Tally) ->
    io:format("~B~n", [Tally]).
