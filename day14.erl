-module(day14).
-compile(export_all).

input() ->
    "498,4 -> 498,6 -> 496,6\n"
    "503,4 -> 502,4 -> 502,9 -> 494,9".

line({X1, Y1}, {X2, Y2}) when X1 == X2 ->
    lists:zip(lists:duplicate(abs(Y1 - Y2) + 1, X1), lists:seq(min(Y1, Y2), max(Y1, Y2)));
line({X1, Y1}, {X2, Y2}) when Y1 == Y2 ->
    lists:zip(lists:seq(min(X1, X2), max(X1, X2)), lists:duplicate(abs(X1 - X2) + 1, Y1)).

initGrid(S) ->
    Lines = [
        [
            list_to_tuple([list_to_integer(I) || I <- string:tokens(Pair, ",")])
         || Pair <- string:tokens(Line, " -> ")
        ]
     || Line <- string:tokens(S, "\n")
    ],
    lists:foldl(
        fun(Points, Acc) ->
            {_, Acc2} = lists:foldl(
                fun(Point, {Last, Map}) ->
                    Line = line(Point, Last),
                    Map2 = lists:foldl(fun(P, Acc2) -> maps:put(P, true, Acc2) end, Map, Line),
                    {Point, Map2}
                end,
                {hd(Points), Acc},
                tl(Points)
            ),
            Acc2
        end,
        maps:new(),
        Lines
    ).

simulateSingle({X, Y} = P, Grid, MaxY) ->
    Down = maps:get({X, Y + 1}, Grid, false),
    Left = maps:get({X - 1, Y + 1}, Grid, false),
    Right = maps:get({X + 1, Y + 1}, Grid, false),
    {_, Y2} =
        Next =
        case {Down, Left, Right} of
            {false, _, _} -> {X, Y + 1};
            {_, false, _} -> {X - 1, Y + 1};
            {_, _, false} -> {X + 1, Y + 1};
            _ -> {X, Y}
        end,
    case {Next == P, Y2 > MaxY} of
        {true, _} -> {maps:put(Next, true, Grid), false};
        {_, true} -> {maps:put(Next, true, Grid), true};
        {_, false} -> simulateSingle(Next, Grid, MaxY)
    end.

maxY(Grid) ->
    lists:foldl(
        fun({_, Y}, Max) ->
            case Y > Max of
                true -> Y;
                false -> Max
            end
        end,
        0,
        maps:keys(Grid)
    ).

simulateAll(Grid, MaxY, N) ->
    {Grid2, Stop} = simulateSingle({500, 0}, Grid, MaxY),
    % stop when any sand reaches MaxY
    case Stop of
        true -> N;
        false -> simulateAll(Grid2, MaxY, N + 1)
    end.

simulateAll2(Grid, MaxY, N) ->
    {Grid2, _} = simulateSingle({500, 0}, Grid, MaxY),
    % stop when {500, 0} is blocked
    case maps:get({500, 0}, Grid2, false) of
        true -> N + 1;
        false -> simulateAll2(Grid2, MaxY, N + 1)
    end.

part1() ->
    S = input(),
    Grid = initGrid(S),
    MaxY = maxY(Grid),
    N = simulateAll(Grid, MaxY, 0),
    erlang:display(N).

part2() ->
    S = input(),
    Grid = initGrid(S),
    MaxY = maxY(Grid),
    N = simulateAll2(Grid, MaxY, 0),
    erlang:display(N).
