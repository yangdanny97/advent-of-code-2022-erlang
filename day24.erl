-module(day24).
-compile(export_all).

input() ->
    "#.######\n"
    "#>>.<^<#\n"
    "#.<..<<#\n"
    "#>v.><>#\n"
    "#<^v^^>#\n"
    "######.#".

input2() ->
    "#.########################################################################################################################\n"
    "#><..<.<<^v><<^>>>^^^<v<>..^<<^<<^^<.<vv><vv<v^>.<>^<v>.<>^v>>>v^><.>.<<>>>^v<>vv<vv>^v^vvv>vv<<v><^<>>^^<><^^><.^v>.^>.<#\n"
    "#>>v<vv<<<<<v<^><<>v.^>^<<<.<^.<<^<.^v^<^^>v<v>v^<v^^v<><>^>>><<^v^^<<v>v^><...^<<<><>>^v>^>><^<<.^v^>v>>>^><vv>^v^.^v><<#\n"
    "#>^^<>^<<<v>^>vv.>^.^<vv^vvv>vv<.^>><v<^>.v..^^^^.>.<<>.<vv.^v^>^^>^.>^.^^^..>>><^<^>><^<<<><v><.^v<^v.>^<v^^.<^>>>>v.^v<#\n"
    "#.>v<<<.vv^v><vv^.<.<<<vvv^.<^^v^<v>^v<.<^^>^.v>>v.<>v<>.<v<^<^v.>^.v^v>^<>>>>vv>v<><<<<><>>><vv<v>><<.v^>.v^vv^>^v<><<^>#\n"
    "#<v>^<<v>v^>v<<><^vvv<<^.>v<v^>vv^>.vv^<^><<<<><v^><^..v>vv<^>>^>.v^<vv^^^.^^v>>..v>>.<^v^>.>>>>>v..^<<^<<vv^^>.v<v<>v<<>#\n"
    "#<>>v>^<.^^^^<<v<<>><<^^>^^.^^^^v<^^<><v>>.>vv^^v>v>v<v^^^>v^vv<<vv>.<vv>^.>v<v<<<v^^.v<<<<v^<^^.^v<vvv^><<>^>^><v<^vv.^<#\n"
    "#<>^vv^>>v>v<^>^<^.^.^<.v>v.^<<<<v><>v.<<<..>.<>>^.^vv^v<^^v.vv<>>vvvv>.^<v^<<<v>^^vv<^v^v<.><v>^.<v^<^>^v<^vv><><<^^.<>.#\n"
    "#.<><>^^<^v^^>v<><><>^.<^<<.<^vvv^^.<.^.^<.^^><<.>><>>>>^^<>v>..v<v>>^>vv.>v^>>v.>^..<>..vv>^<vv>>><^.vv^><>^^^^vvv^v^^<<#\n"
    "#<^>.<^^<^v<><v.<>^<^.>>>>^<.>vv^.v^<>v>^<<v<<<v.^v>vvvv<>^<<<<.v.v><<.^v^<v>.<^<<^<v<<<^><v<v>>>^<^^<^^^^vv<v<^>^<vv>v^<#\n"
    "#><v>.>.v><<^>v^><.^<<>^v>>.<^^^<<<><<<^<^^v<.>^vv.>^vvv^v<<v>vv<<.^v^vv>.v^v><v<v.^.<.vv>^>>^>>^v^^^.><^v<v^<v>><>.>v^<<#\n"
    "#>>>>vv><^<vv>>^<v.><>><^^<v^>^^<vv>><v>>><>>vvv<^.<^v>^.<<v.<..^v^^<v<v^>vv^<^.^^^<.^vv..<<>><^<v<^>^^^<^^v^>^>>v><><<.<#\n"
    "#>v>><>.v>^><<^.>>>vvv^<.v<>vv^<^^<<vv<>v^<.^v>.v>^<v<<vv<^vv<.<>>^.v^.>><..<v^^>^^^^<>^^.^.>vv^<v^^.vv..<^^v<vv.^<^^v.>>#\n"
    "#<>^^vvvv^><>^vv<><.<<<>^.<.v^<<vv^v^vvvv<>>v<^^v>^^v>^>^v<<.>.<>.^v.<^<<.^v^^><v<<^v<^><><^>>>.<<<v..v><>^vvv<^.<v<.>><.#\n"
    "#<<^<>v>v^v<v^v^^^>.v>..^^<>vv<<^v>^<.^^.vv<.<.<<<<vv><><..>v><>>>v.v<^^>v>>^<v.<<.>v<^<^^>v>>>v^>^v><><<^<^.vvvv>>^>^..>#\n"
    "#<><>^^.<<v<^v>^^.^v.v<>^^vvv<<<<^>^<><.vv<v^<v.^.v^vv^<.^^^<>v.^<.>^>.>.^<<<<<^^<^.^.^>.^>.>>vvv<.<v<.vv><>^><>><^<>.<^<#\n"
    "#<<vv^^^<>^^^<vv<vv^<^v.>v<>>>vvv><^<<<>^^^.<<.vv><><^<<v>>>^><..<v.v>>^<<>>>>^>v^.>.><v>>^^vv<>v.^^vv<>>^<^>v>vv><<>><^>#\n"
    "#>v.<<><^<>.>><>><.^^v>^^v^>>vv>vv>>><^...^vv^v^.>vv..<vv<<<^^^><^<v.><>v.v^^^^v>^^<^<<^v>^>.v^>.<^<<>^^.<<<>^<v.^v^.><^>#\n"
    "#><^^>>^<>^<>>vv>^.^vvv^<vv><>^^^<<v^^>v<^>v<<^v^><^v<>v^.^><>v<^vv>v<>v<><>^>>.<^>^>.<vv<>>^.<^<^vvv.v<v<v>^v^^>.>v>.^<>#\n"
    "#<<<<v....>^><>v^>v>.<.>v<^<v<<^^>>>^^vv<^vv^^^<<<<>><<^<<^v>v.>..v>v<>vv.>v<v>^^^v<^^^vvvvv^<.>vv^^.vv>v<.^^<<<<v^<>^v^>#\n"
    "#<<<vvv^^.<v^^<.vv^>v^<<v<<v.<^<<^>^<v<<>.^>.><v^>>^^^v<v.<^v>^<>>.^^><^<<.>v^v>.<>.^vv.^>^v.^^^^.<.vv^vv<vv^<<<^><vvv><<#\n"
    "#.vv>^.vv<<<<.<vv^.^^>v.><v^^v<<>v><.<v^><v<vv^v<><<<>.>v^^.^^<v>>^<^v^^<^^v<>^^v>><>.><^<^^>>v<<vv<v>^^^^><><vv<>><<<>^<#\n"
    "#>vv^>.^<><<..<v^vv>>^<><.v^<^>^><v>v.v^^<<>>v<vv><^v>.<<vv>v^.<<v^^^^v^<.>.>v.>>v..<>><v.^^<v<^v>v^^v^<<^>>>v>>^v<^<>.<.#\n"
    "#>v^<^.v^>><<><^<v<^^<v>^^..><<>vv^^><vv><<<.><.<^.<<<>v<.>vv<<v^.^v^.<v.vv>>>^v>.^>.^><^^vv<>v<v>>><^>^^^vvv^v.v>v<<<>v<#\n"
    "#>^v<>.>.><^>>vv.>v^><>><<>v^^v>^^vv>v>^.<<v<.v<^<<^^v<vvv<^^>^^<>^>^v^vv.><<^>.<>^>v<^<.>>vvv>>>v^<>vv.<>v>^^>vv^>>vv<v<#\n"
    "#<v>vv.vv^>>v^v<<>^<v^<<<<^<.^vvv<v^^>><<<<>v>^<v<<<v>><^<..>.v^>>vv^>>>>v.v.>^^^<v<^>^^<^<^<><v>^>>>>..^vv>.^<>v^>>v>.><#\n"
    "########################################################################################################################.#".

check({X, Y} = P, {MinX, MaxX, MinY, MaxY}, Blizzards) ->
    X >= MinX andalso X =< MaxX andalso Y >= MinY andalso Y =< MaxY andalso
        not lists:member(P, Blizzards).
update(K, V, Map) ->
    maps:put(K, min(maps:get(K, Map, 9999), V), Map).

% https://programming-idioms.org/idiom/75/compute-lcm/2484/erlang
gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).
lcm(A,B) -> (A*B) div gcd(A, B).

search(Current, Destination, Move, _, _, _, Distances) when Current == Destination ->
    update(Destination, Move, Distances);
search(
    {X, Y} = Current,
    Destination,
    Move,
    Lcm,
    {MinX, MaxX, MinY, MaxY} = Bounds,
    Blizzards,
    Distances
) ->
    D1 = maps:get(Current, Distances, 9999),
    D2 = maps:get(Destination, Distances, 9999),
    if
        D1 =< Move - Lcm ->
            Distances;
        D2 =< Move ->
            Distances;
        true ->
            Distances2 = update(Current, Move, Distances),
            Blizzards2 = lists:map(
                fun({{Bx, By}, {Dx, Dy} = D}) ->
                    B2 = {Bx + Dx, By + Dy},
                    case check(B2, Bounds, []) of
                        true ->
                            {B2, D};
                        false ->
                            B3 =
                                case D of
                                    {1, 0} -> {MinX, By};
                                    {-1, 0} -> {MaxX, By};
                                    {0, 1} -> {Bx, MinY};
                                    {0, -1} -> {Bx, MaxY}
                                end,
                            {B3, D}
                    end
                end,
                Blizzards
            ),
            BCoords = [B || {B, _} <- Blizzards2],
            Dirs = [{0, 1}, {1, 0}, {-1, 0}, {0, -1}, {0, 0}],
            lists:foldl(
                fun({Dx, Dy}, Acc) ->
                    Next = {X + Dx, Y + Dy},
                    case
                        check(Next, Bounds, BCoords) orelse (Next == {MinX, MinY - 1}) orelse
                            (Next == {MaxX, MaxY + 1})
                    of
                        true ->
                            search(
                                Next, Destination, Move + 1, Lcm, Bounds, Blizzards2, Acc
                            );
                        false ->
                            Acc
                    end
                end,
                Distances2,
                Dirs
            )
    end.

processInput(Input) ->
    Lines = string:tokens(Input, "\n"),
    lists:foldl(
        fun({Y, Line}, Acc) ->
            lists:foldl(
                fun({X, Char}, {{MinX, MaxX, MinY, MaxY}=Bounds, Blizzards}) ->
                    B2 = {min(X, MinX), max(X, MaxX), min(Y, MinY), max(Y, MaxY)},
                    case [Char] of
                            ">" -> {B2, [{{X, Y}, {1, 0}} | Blizzards]};
                            "v" -> {B2, [{{X, Y}, {0, 1}} | Blizzards]};
                            "<" -> {B2, [{{X, Y}, {-1, 0}} | Blizzards]};
                            "^" -> {B2, [{{X, Y}, {0, -1}} | Blizzards]};
                            _ -> {Bounds, Blizzards}
                    end
                end,
                Acc,
                lists:zip(lists:seq(1, length(Line)), Line)
            )
        end,
        {{2, 2, 2, 2}, []},
        lists:zip(lists:seq(1, length(Lines)), Lines)
    ).

part1() ->
    {{MinX, MaxX, MinY, MaxY} = Bounds, Blizzards} = processInput(input2()),
    Start = {MinX, MinY - 1},
    Dest = {MaxX, MaxY + 1},
    {W, H} = {MaxX - MinX + 1, MaxY - MinY + 1},
    Lcm = lcm(W, H),
    erlang:display(Lcm),
    Distances = search(Start, Dest, 0, Lcm, Bounds, Blizzards, maps:new()),
    erlang:display(Distances),
    erlang:display(maps:get(Dest, Distances)).
