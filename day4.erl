-module(day4).
-compile(export_all).

input() ->
    "2-4,6-8\n"
    "2-3,4-5\n"
    "5-7,7-9\n"
    "2-8,3-7\n"
    "6-6,4-6\n"
    "2-6,4-8\n".

part1() ->
    S = input(),
    Split = string:tokens(S, "\n"),
    Sum = lists:foldl(
        fun(X, Sum) ->
            Assignments = [
                list_to_integer(L)
             || L <- string:tokens(X, "-,")
            ],
            Sum +
                case Assignments of
                    [A, B, C, D] when (A =< C) and (B >= D) -> 1;
                    [A, B, C, D] when (A >= C) and (B =< D) -> 1;
                    _ -> 0
                end
        end,
        0,
        Split
    ),
    erlang:display(Sum).

part2() ->
    S = input(),
    Split = string:tokens(S, "\n"),
    Sum = lists:foldl(
        fun(X, Sum) ->
            Assignments = [
                list_to_integer(L)
             || L <- string:tokens(X, "-,")
            ],
            Sum +
                case Assignments of
                    [A, _, C, D] when (A >= C) and (A =< D) -> 1;
                    [_, B, C, D] when (B >= C) and (B =< D) -> 1;
                    [A, B, C, D] when (A =< C) and (B >= D) -> 1;
                    [A, B, C, D] when (A >= C) and (B =< D) -> 1;
                    _ -> 0
                end
        end,
        0,
        Split
    ),
    erlang:display(Sum).
