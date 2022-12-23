-module(day23).
-compile(export_all).

input() ->
    "....#..\n"
    "..###.#\n"
    "#...#.#\n"
    ".#...##\n"
    "#.###..\n"
    "##.#.##\n"
    ".#..#..".

makeMap(Input) ->
    Lines = string:tokens(Input, "\n"),
    lists:foldl(
        fun({Y, Line}, Acc) ->
            lists:foldl(
                fun({X, Char}, Acc2) ->
                    case [Char] of
                        "#" -> maps:put({X, Y}, true, Acc2);
                        _ -> Acc2
                    end
                end,
                Acc,
                lists:zip(lists:seq(1, length(Line)), Line)
            )
        end,
        maps:new(),
        lists:zip(lists:seq(1, length(Lines)), Lines)
    ).

check({X, Y}, {Dx, Dy}, Map) ->
    maps:get({X + Dx, Y + Dy}, Map, false).

turn(N, Map) ->
    D = [
        {{0, -1}, [{0, -1}, {-1, -1}, {1, -1}]},
        {{0, 1}, [{0, 1}, {-1, 1}, {1, 1}]},
        {{-1, 0}, [{-1, -1}, {-1, 0}, {-1, 1}]},
        {{1, 0}, [{1, -1}, {1, 0}, {1, 1}]}
    ],
    Neighbors = [{0, 1}, {-1, 1}, {1, 1}, {0, -1}, {-1, -1}, {1, -1}, {1, 0}, {-1, 0}],
    M = (N - 1) rem 4,
    Directions = lists:sublist(D, M + 1, 4) ++ lists:sublist(D, M),
    Proposals = maps:map(
        fun({X, Y} = P, _) ->
            HasNeighbors = lists:any(fun(Ne) -> check(P, Ne, Map) end, Neighbors),
            case HasNeighbors of
                true ->
                    lists:foldl(
                        fun({{Dx, Dy}, Check}, Acc) ->
                            case Acc of
                                A when A == P ->
                                    case lists:any(fun(Ne) -> check(P, Ne, Map) end, Check) of
                                        true -> Acc;
                                        false -> {X + Dx, Y + Dy}
                                    end;
                                _ ->
                                    Acc
                            end
                        end,
                        P,
                        Directions
                    );
                false ->
                    P
            end
        end,
        Map
    ),
    PMap = lists:foldl(
        fun(P, PMap) ->
            maps:put(P, maps:get(P, PMap, 0) + 1, PMap)
        end,
        maps:new(),
        maps:values(Proposals)
    ),
    maps:fold(
        fun(Original, Proposed, Acc) ->
            case maps:get(Proposed, PMap, 1) of
                1 -> maps:put(Proposed, true, Acc);
                _ -> maps:put(Original, true, Acc)
            end
        end,
        maps:new(),
        Proposals
    ).

rect(Map) ->
    K = maps:keys(Map),
    Xs = [X || {X, _} <- K],
    Ys = [Y || {_, Y} <- K],
    (lists:max(Xs) - lists:min(Xs) + 1) * (lists:max(Ys) - lists:min(Ys) + 1).

simulate(N, Map)->
    Map2 = turn(N, Map),
    Start = ordsets:from_list(maps:keys(Map)),
    End = ordsets:from_list(maps:keys(Map2)),
    case Start == End of 
        true -> N;
        false -> simulate(N + 1, Map2)
    end.

part1() ->
    Map = makeMap(input()),
    Result = lists:foldl(fun(N, Acc) -> turn(N, Acc) end, Map, lists:seq(1, 10)),
    erlang:display(rect(Result) - length(maps:keys(Result))).

part2()->
    Map = makeMap(input()),
    erlang:display(simulate(1, Map)).