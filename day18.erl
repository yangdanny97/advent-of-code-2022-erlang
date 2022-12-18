-module(day18).
-compile(export_all).

input() ->
    "2,2,2\n"
    "1,2,2\n"
    "3,2,2\n"
    "2,1,2\n"
    "2,3,2\n"
    "2,2,1\n"
    "2,2,3\n"
    "2,2,4\n"
    "2,2,6\n"
    "1,2,5\n"
    "3,2,5\n"
    "2,1,5\n"
    "2,3,5".

part1() ->
    S = input(),
    Points = [
        list_to_tuple([list_to_integer(I) || I <- string:tokens(L, ",")])
     || L <- string:tokens(S, "\n")
    ],
    PMap = maps:from_keys(Points, 0),
    Sides = lists:foldl(
        fun({X, Y, Z}, Sum) ->
            Neighbors = [
                {X + 1, Y, Z},
                {X - 1, Y, Z},
                {X, Y + 1, Z},
                {X, Y - 1, Z},
                {X, Y, Z + 1},
                {X, Y, Z - 1}
            ],
            Sum + lists:sum([maps:get(N, PMap, 1) || N <- Neighbors])
        end,
        0,
        Points
    ),
    erlang:display(Sides).

fill({X, _, _}, {MinX, _, _, _, _, _}, _, Acc) when X < MinX -> Acc;
fill({X, _, _}, {_, MaxX, _, _, _, _}, _, Acc) when X > MaxX -> Acc;
fill({_, Y, _}, {_, _, MinY, _, _, _}, _, Acc) when Y < MinY -> Acc;
fill({_, Y, _}, {_, _, _, MaxY, _, _}, _, Acc) when Y > MaxY -> Acc;
fill({_, _, Z}, {_, _, _, _, MinZ, _}, _, Acc) when Z < MinZ -> Acc;
fill({_, _, Z}, {_, _, _, _, _, MaxZ}, _, Acc) when Z > MaxZ -> Acc;
fill({X, Y, Z} = P, Bounds, Points, Acc) ->
    case {maps:get(P, Points, false), maps:get(P, Acc, false)} of
        {false, false} ->
            Neighbors = [
                {X + 1, Y, Z},
                {X - 1, Y, Z},
                {X, Y + 1, Z},
                {X, Y - 1, Z},
                {X, Y, Z + 1},
                {X, Y, Z - 1}
            ],
            lists:foldl(
                fun(P2, Acc2) ->
                    fill(P2, Bounds, Points, Acc2)
                end,
                maps:put(P, 1, Acc),
                Neighbors
            );
        _ ->
            Acc
    end.

part2() ->
    S = input(),
    Points = [
        list_to_tuple([list_to_integer(I) || I <- string:tokens(L, ",")])
     || L <- string:tokens(S, "\n")
    ],
    PMap = maps:from_keys(Points, true),
    Xs = lists:map(fun({X, _, _}) -> X end, Points),
    Ys = lists:map(fun({_, Y, _}) -> Y end, Points),
    Zs = lists:map(fun({_, _, Z}) -> Z end, Points),
    MinX = lists:min(Xs),
    MinY = lists:min(Ys),
    MinZ = lists:min(Zs),
    % start outside the rock and try to fill a bounding box around the rock
    OMap = fill(
        {MinX - 1, MinY - 1, MinZ - 1},
        {MinX - 1, lists:max(Xs) + 1, MinY - 1, lists:max(Ys) + 1, MinZ - 1, lists:max(Zs) + 1},
        PMap,
        maps:new()
    ),
    Sides = lists:foldl(
        fun({X, Y, Z}, Sum) ->
            Neighbors = [
                {X + 1, Y, Z},
                {X - 1, Y, Z},
                {X, Y + 1, Z},
                {X, Y - 1, Z},
                {X, Y, Z + 1},
                {X, Y, Z - 1}
            ],
            Sum + lists:sum([maps:get(N, OMap, 0) || N <- Neighbors])
        end,
        0,
        Points
    ),
    erlang:display(Sides).
