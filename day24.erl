-module(day24).
-compile(export_all).

input() ->
    "#.######\n"
    "#>>.<^<#\n"
    "#.<..<<#\n"
    "#>v.><>#\n"
    "#<^v^^>#\n"
    "######.#".

check({X, Y} = P, {MinX, MaxX, MinY, MaxY}, Blizzards) ->
    X >= MinX andalso X =< MaxX andalso Y >= MinY andalso Y =< MaxY andalso
        not lists:member(P, Blizzards).

% instead of doing a tree search, keep track of reachable coordinates on each turn
search(Move, Accessible, Blizzards, {MinX, MaxX, MinY, MaxY}=Bounds, Destination)->
    case lists:member(Destination, Accessible) of
        true -> {Move, Blizzards};
        false -> 
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
            Accessible2 = lists:flatten(
                lists:map(
                    fun({X, Y})->
                        Mapped = [{X + Dx, Y + Dy} || {Dx, Dy} <- Dirs],
                        lists:filter(fun(Next)-> 
                        check(Next, Bounds, BCoords) orelse (Next == {MinX, MinY - 1}) orelse
                            (Next == {MaxX, MaxY + 1})
                        end, Mapped)
                    end,
                Accessible
                )
                
            ),
            search(Move + 1, ordsets:from_list(Accessible2), Blizzards2, Bounds, Destination)
    end.

% get the bounds of the legal coordinates & a list of blizzard positions/directions
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
    {{MinX, MaxX, MinY, MaxY} = Bounds, Blizzards} = processInput(input()),
    Start = {MinX, MinY - 1},
    Dest = {MaxX, MaxY + 1},
    {N, _} = search(0, [Start], Blizzards, Bounds, Dest),
    erlang:display(N).

part2() ->
    {{MinX, MaxX, MinY, MaxY} = Bounds, Blizzards} = processInput(input()),
    Start = {MinX, MinY - 1},
    Dest = {MaxX, MaxY + 1},
    {N, B} = search(0, [Start], Blizzards, Bounds, Dest),
    {N2, B2} = search(N, [Dest], B, Bounds, Start),
    {N3, _} = search(N2, [Start], B2, Bounds, Dest),
    erlang:display(N3).
