-module(day4).
-compile(export_all).

part1() ->
    S =
        "\n"
        "2-4,6-8\n"
        "2-3,4-5\n"
        "5-7,7-9\n"
        "2-8,3-7\n"
        "6-6,4-6\n"
        "2-6,4-8\n",
    Split = [X || X <- string:split(S ++ "\n", "\n", all), not string:equal(X, "")],
    Sum = lists:foldl(
        fun(X, Sum) ->
            Assignments = [
                list_to_integer(L)
             || L <- string:split(string:replace(X, ",", "-"), "-", all)
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
    io:format("~w~n", [Sum]).

part2() ->
    S =
        "\n"
        "2-4,6-8\n"
        "2-3,4-5\n"
        "5-7,7-9\n"
        "2-8,3-7\n"
        "6-6,4-6\n"
        "2-6,4-8\n",
    Split = [X || X <- string:split(S ++ "\n", "\n", all), not string:equal(X, "")],
    Sum = lists:foldl(
        fun(X, Sum) ->
            Assignments = [
                list_to_integer(L)
             || L <- string:split(string:replace(X, ",", "-"), "-", all)
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
    io:format("~w~n", [Sum]).
