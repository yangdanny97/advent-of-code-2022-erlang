-module(day12).
-compile(export_all).

input() ->
    "Sabqponm\n"
    "abcryxxl\n"
    "accszExk\n"
    "acctuvwj\n"
    "abdefghi".

score("S") -> 1;
score("E") -> 26;
score(L) -> hd(L) - 96.

find(S, C) ->
    NL = string:chr(S, $\n),
    Pos = string:chr(S, C),
    {Pos rem NL, (Pos div NL) + 1}.

search({X, Y} = C, Dist, EMap, DMap) ->
    Neighbors = [{-1, 0}, {1, 0}, {0, 1}, {0, -1}],
    E = maps:get(C, EMap),
    lists:foldl(
        fun({Nx, Ny}, Acc) ->
            Next = {X + Nx, Y + Ny},
            case {maps:get(Next, EMap, -1), maps:get(Next, DMap, -1)} of
                {-1, _} ->
                    Acc;
                {ENext, DNext} when (ENext >= (E - 1)) and (DNext > Dist) ->
                    Acc2 = maps:put(Next, Dist, Acc),
                    search(Next, Dist + 1, EMap, Acc2);
                _ ->
                    Acc
            end
        end,
        DMap,
        Neighbors
    ).

grid(Lines) ->
    maps:from_list(
        lists:flatten([
            [
                {{X, Y}, score([Val])}
             || {X, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
            ]
         || {Y, Row} <- lists:zip(lists:seq(1, length(Lines)), Lines)
        ])
    ).

part1() ->
    S = input(),
    Lines = string:tokens(S, "\n"),
    Start = find(S, $S),
    End = find(S, $E),
    % {X, Y} => Elevation
    EMap = grid(Lines),
    % {X, Y} => Distance from end
    DMap = maps:put(End, 0, maps:map(fun(_, _) -> length(S) end, EMap)),
    Result = search(End, 1, EMap, DMap),
    erlang:display(maps:get(Start, Result)).

part2() ->
    S = input(),
    Lines = string:tokens(S, "\n"),
    End = find(S, $E),
    % {X, Y} => Elevation
    EMap = grid(Lines),
    % {X, Y} => Distance from end
    DMap = maps:put(End, 0, maps:map(fun(_, _) -> length(S) end, EMap)),
    Result = search(End, 1, EMap, DMap),
    ValidStarts = maps:filter(fun(K, _) -> maps:get(K, EMap) == 1 end, Result),
    erlang:display(hd(lists:sort(maps:values(ValidStarts)))).
