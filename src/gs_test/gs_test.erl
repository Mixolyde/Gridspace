-module(test_util).
-author("mixolyde@gmail.com").
-export([test_avg/4]).

test_avg(M, F, A, N) when N > 0 ->
    L = test_loop(M, F, A, N, []),
    Length = length(L),
    Min = lists:min(L),
    Max = lists:max(L),
    Med = lists:nth(round((Length / 2)), lists:sort(L)),
    Avg = round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) / Length),
    io:format("Range: ~b - ~b mics~n"
          "Median: ~b mics~n"
          "Average: ~b mics~n",
          [Min, Max, Med, Avg]),
    Med.

test_loop(_M, _F, _A, 0, List) ->
    List;
test_loop(M, F, A, N, List) ->
    {T, _Result} = timer:tc(M, F, A),
    test_loop(M, F, A, N - 1, [T|List]).

supervise_tests([]) -> ok;
supervise_tests([TestFun | RestFuns]) ->

    %try the test function
    TestFun,
    %catch exceptions

    %try the next
    supervise_tests(RestFuns).

%Which process uses the most memory:
% lists:reverse(lists:keysort(2,[{P, erlang:process_info(P, heap_size)} || P <- erlang:processes()])).

%Which ets table uses the most memory:
% lists:reverse(lists:keysort(2,[{T, ets:info(T, memory)} || T <- ets:all()])).