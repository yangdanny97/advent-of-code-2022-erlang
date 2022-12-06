-module(day2).
-compile(export_all).

part1() ->
    S =
        "A Y\n"
        "B X\n"
        "C Z\n",
    Split = string:tokens(S, "\n"),
    Mapped = lists:map(
        fun(X) -> string:split(X, " ") end,
        Split
    ),
    Score = lists:foldl(
        fun(X, Acc) ->
            Acc +
                case X of
                    ["A", "X"] -> 1 + 3;
                    ["A", "Y"] -> 2 + 6;
                    ["A", "Z"] -> 3 + 0;
                    ["B", "X"] -> 1 + 0;
                    ["B", "Y"] -> 2 + 3;
                    ["B", "Z"] -> 3 + 6;
                    ["C", "X"] -> 1 + 6;
                    ["C", "Y"] -> 2 + 0;
                    ["C", "Z"] -> 3 + 3;
                    _ -> 0
                end
        end,
        0,
        Mapped
    ),
    erlang:display(Score).

part2() ->
    S =
        "A Y\n"
        "B X\n"
        "C Z\n",
    Split = string:tokens(S, "\n"),
    Mapped = lists:map(
        fun(X) -> string:split(X, " ") end,
        Split
    ),
    Score = lists:foldl(
        fun(X, Acc) ->
            Acc +
                case X of
                    ["A", "X"] -> 0 + 3;
                    ["A", "Y"] -> 3 + 1;
                    ["A", "Z"] -> 6 + 2;
                    ["B", "X"] -> 0 + 1;
                    ["B", "Y"] -> 3 + 2;
                    ["B", "Z"] -> 6 + 3;
                    ["C", "X"] -> 0 + 2;
                    ["C", "Y"] -> 3 + 3;
                    ["C", "Z"] -> 6 + 1;
                    _ -> 0
                end
        end,
        0,
        Mapped
    ),
    erlang:display(Score).
