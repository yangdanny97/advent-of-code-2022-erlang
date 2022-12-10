-module(day10).
-compile(export_all).

input() ->
    "addx 15\n"
    "addx -11\n"
    "addx 6\n"
    "addx -3\n"
    "addx 5\n"
    "addx -1\n"
    "addx -8\n"
    "addx 13\n"
    "addx 4\n"
    "noop\n"
    "addx -1\n"
    "addx 5\n"
    "addx -1\n"
    "addx 5\n"
    "addx -1\n"
    "addx 5\n"
    "addx -1\n"
    "addx 5\n"
    "addx -1\n"
    "addx -35\n"
    "addx 1\n"
    "addx 24\n"
    "addx -19\n"
    "addx 1\n"
    "addx 16\n"
    "addx -11\n"
    "noop\n"
    "noop\n"
    "addx 21\n"
    "addx -15\n"
    "noop\n"
    "noop\n"
    "addx -3\n"
    "addx 9\n"
    "addx 1\n"
    "addx -3\n"
    "addx 8\n"
    "addx 1\n"
    "addx 5\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx -36\n"
    "noop\n"
    "addx 1\n"
    "addx 7\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx 2\n"
    "addx 6\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx 1\n"
    "noop\n"
    "noop\n"
    "addx 7\n"
    "addx 1\n"
    "noop\n"
    "addx -13\n"
    "addx 13\n"
    "addx 7\n"
    "noop\n"
    "addx 1\n"
    "addx -33\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx 2\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx 8\n"
    "noop\n"
    "addx -1\n"
    "addx 2\n"
    "addx 1\n"
    "noop\n"
    "addx 17\n"
    "addx -9\n"
    "addx 1\n"
    "addx 1\n"
    "addx -3\n"
    "addx 11\n"
    "noop\n"
    "noop\n"
    "addx 1\n"
    "noop\n"
    "addx 1\n"
    "noop\n"
    "noop\n"
    "addx -13\n"
    "addx -19\n"
    "addx 1\n"
    "addx 3\n"
    "addx 26\n"
    "addx -30\n"
    "addx 12\n"
    "addx -1\n"
    "addx 3\n"
    "addx 1\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx -9\n"
    "addx 18\n"
    "addx 1\n"
    "addx 2\n"
    "noop\n"
    "noop\n"
    "addx 9\n"
    "noop\n"
    "noop\n"
    "noop\n"
    "addx -1\n"
    "addx 2\n"
    "addx -37\n"
    "addx 1\n"
    "addx 3\n"
    "noop\n"
    "addx 15\n"
    "addx -21\n"
    "addx 22\n"
    "addx -6\n"
    "addx 1\n"
    "noop\n"
    "addx 2\n"
    "addx 1\n"
    "noop\n"
    "addx -10\n"
    "noop\n"
    "noop\n"
    "addx 20\n"
    "addx 1\n"
    "addx 2\n"
    "addx 2\n"
    "addx -6\n"
    "addx -11\n"
    "noop\n"
    "noop\n"
    "noop".

part1() ->
    S = input(),
    Instructions = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, _, _, Sum} = lists:foldl(
        fun(I, {X, Instrs, IsExec, Sum}) ->
            Save = ((I - 20) rem 40 == 0) and (I =< 220),
            Sum2 =
                case Save of
                    true -> Sum + X * I;
                    false -> Sum
                end,
            {X2, Instrs2, IsExec2} =
                case {IsExec, Instrs} of
                    {true, [["addx", N] | T]} -> {X + list_to_integer(N), T, false};
                    {false, [["addx", _] | _]} -> {X, Instrs, true};
                    {_, [_ | T]} -> {X, T, false};
                    {_, _} -> {X, Instrs, false}
                end,
            {X2, Instrs2, IsExec2, Sum2}
        end,
        {1, Instructions, false, 0},
        lists:seq(1, 220)
    ),
    erlang:display(Sum).

part2() ->
    S = input(),
    Instructions = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, _, _, Result} = lists:foldl(
        fun(I, {X, Instrs, IsExec, Result}) ->
            Cursor = (I - 1) rem 40,
            Visible = abs(Cursor - X) =< 1,
            EOL = Cursor == 39,
            Result2 =
                case {Visible, EOL} of
                    {true, true} -> "\n#" ++ Result;
                    {false, true} -> "\n." ++ Result;
                    {true, false} -> "#" ++ Result;
                    {false, false} -> "." ++ Result
                end,
            {X2, Instrs2, IsExec2} =
                case {IsExec, Instrs} of
                    {true, [["addx", N] | T]} -> {X + list_to_integer(N), T, false};
                    {false, [["addx", _] | _]} -> {X, Instrs, true};
                    {_, [_ | T]} -> {X, T, false};
                    {_, _} -> {X, Instrs, false}
                end,
            {X2, Instrs2, IsExec2, Result2}
        end,
        {1, Instructions, false, ""},
        lists:seq(1, 240)
    ),
    lists:foreach(fun(X) -> erlang:display(X) end, string:tokens(lists:reverse(Result), "\n")).
