-module(day9).
-compile(export_all).

isAdj({X1, Y1}, {X2, Y2}) ->
    (abs(X1 - X2) =< 1) and (abs(Y1 - Y2) =< 1).

% based on the position of the head, move the rest of the rope
moveHelper([H]) ->
    {[H], H};
moveHelper([{Hx, Hy} = H, {Tx, Ty} = T | Rest]) ->
    IsAdj = isAdj(H, T),
    {Tx2, Ty2} =
        if
            IsAdj -> T;
            (Hx == Tx) and (Hy > Ty) -> {Tx, Ty + 1};
            (Hx == Tx) and (Hy < Ty) -> {Tx, Ty - 1};
            (Hx > Tx) and (Hy == Ty) -> {Tx + 1, Ty};
            (Hx < Tx) and (Hy == Ty) -> {Tx - 1, Ty};
            (Hx > Tx) and (Hy > Ty) -> {Tx + 1, Ty + 1};
            (Hx < Tx) and (Hy > Ty) -> {Tx - 1, Ty + 1};
            (Hx > Tx) and (Hy < Ty) -> {Tx + 1, Ty - 1};
            (Hx < Tx) and (Hy < Ty) -> {Tx - 1, Ty - 1}
        end,
    {Rest2, TPos} = moveHelper([{Tx2, Ty2} | Rest]),
    {[H | Rest2], TPos}.

% move the head N times, repositioning the entire rope each time
moveN([{Hx, Hy}, T | Rest], Dx, Dy, N, TVisited) ->
    H2 = {Hx + Dx, Hy + Dy},
    {Rope, TPos} = moveHelper([H2, T | Rest]),
    case N of
        1 -> {Rope, [TPos | TVisited]};
        _ -> moveN(Rope, Dx, Dy, N - 1, [TPos | TVisited])
    end.

processInstr([Dir, Times], [_, _ | _] = Rope) ->
    N = list_to_integer(Times),
    case Dir of
        "U" -> moveN(Rope, 0, 1, N, []);
        "D" -> moveN(Rope, 0, -1, N, []);
        "L" -> moveN(Rope, -1, 0, N, []);
        "R" -> moveN(Rope, 1, 0, N, [])
    end.

part1() ->
    S =
        "R 4\n"
        "U 4\n"
        "L 3\n"
        "D 1\n"
        "R 4\n"
        "D 1\n"
        "L 5\n"
        "R 2",
    Instrs = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, _, Visited} = lists:foldl(
        fun(Instr, {H, T, V}) ->
            {[H2, T2], P} = processInstr(Instr, [H, T]),
            {H2, T2, P ++ V}
        end,
        {{0, 0}, {0, 0}, [{0, 0}]},
        Instrs
    ),
    erlang:display(length(ordsets:from_list(Visited))).

part2() ->
    S =
        "R 5\n"
        "U 8\n"
        "L 8\n"
        "D 3\n"
        "R 17\n"
        "D 10\n"
        "L 25\n"
        "U 20",
    Instrs = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, Visited} = lists:foldl(
        fun(Instr, {Rope, V}) ->
            {Rope2, P} = processInstr(Instr, Rope),
            {Rope2, P ++ V}
        end,
        {lists:duplicate(10, {0, 0}), [{0, 0}]},
        Instrs
    ),
    erlang:display(length(ordsets:from_list(Visited))).
