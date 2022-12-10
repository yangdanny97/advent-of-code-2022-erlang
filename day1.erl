-module(day1).
-compile(export_all).

input() ->
    "1000\n"
    "2000\n"
    "3000\n"
    "\n"
    "4000\n"
    "\n"
    "5000\n"
    "6000\n"
    "\n"
    "7000\n"
    "8000\n"
    "9000\n"
    "\n"
    "10000\n".

part1() ->
    S = input(),
    Sums = lists:map(
        fun(X) ->
            Calories = [list_to_integer(T) || T <- string:tokens(X, "\n")],
            lists:foldl(fun(Y, Acc) -> Acc + Y end, 0, Calories)
        end,
        string:split(S, "\n\n", all)
    ),
    erlang:display(lists:max(Sums)).

part2() ->
    S = input(),
    Sums = lists:map(
        fun(X) ->
            Calories = [list_to_integer(T) || T <- string:tokens(X, "\n")],
            lists:foldl(fun(Y, Acc) -> Acc + Y end, 0, Calories)
        end,
        string:split(S, "\n\n", all)
    ),
    case lists:reverse(lists:sort(Sums)) of
        [N1, N2, N3 | _] -> erlang:display(N1 + N2 + N3)
    end.
