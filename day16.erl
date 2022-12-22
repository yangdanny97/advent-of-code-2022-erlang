-module(day16).
-compile(export_all).

input() ->
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA\n"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB\n"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD\n"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG\n"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH\n"
    "Valve HH has flow rate=22; tunnel leads to valve GG\n"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ\n"
    "Valve JJ has flow rate=21; tunnel leads to valve II".

getFlow(Valve, Map) ->
    {Flow, _} = maps:get(Valve, Map),
    Flow.

processLine(Line, Map) ->
    S = re:replace(Line, "Valve ", "", [global, {return, list}]),
    S2 = re:replace(S, " has flow rate=", ", ", [global, {return, list}]),
    S3 = re:replace(S2, "; tunnels lead to valves ", ", ", [global, {return, list}]),
    S4 = re:replace(S3, "; tunnel leads to valve ", ", ", [global, {return, list}]),
    [Valve, Flow | Tunnels] = string:tokens(S4, ", "),
    maps:put(Valve, {list_to_integer(Flow), ordsets:from_list(Tunnels)}, Map).

% build distance matrix
distance(Start, Current, N, Map, Distances) ->
    Dist = maps:get({Start, Current}, Distances, 9999),
    if
        Dist == 9999 orelse N < Dist ->
            {_, Adj} = maps:get(Current, Map),
            lists:foldl(
                fun(Next, D) ->
                    distance(Start, Next, N + 1, Map, D)
                end,
                maps:put({Start, Current}, N, Distances),
                Adj
            );
        true ->
            Distances
    end.

best(_, T, _, _, _, _, Best) when T =< 0 -> Best;
best(_, _, [], _, _, _, Best) ->
    Best;
best([Pos | _] = Path, TimeLeft, Unopened, Map, Distances, Current, Best) ->
    if
        % heuristic: end early if tree looks bad
        TimeLeft < 20 andalso Current < (Best div 2) ->
            Best;
        true ->
            case lists:member(Pos, Unopened) of
                true ->
                    T2 = TimeLeft - 1,
                    Score = Current + (getFlow(Pos, Map) * T2),
                    best(
                        [Pos | Path],
                        T2,
                        ordsets:del_element(Pos, Unopened),
                        Map,
                        Distances,
                        Score,
                        max(Best, Score)
                    );
                false ->
                    % heuristic: visit unopened valves w/ highest flow first
                    Nexts = lists:sort(
                        fun(A, B) ->
                            getFlow(A, Map) =< getFlow(B, Map)
                        end,
                        Unopened
                    ),
                    lists:foldl(
                        fun(Next, Acc) ->
                            Dist = maps:get({Pos, Next}, Distances),
                            max(
                                Acc,
                                best(
                                    [Next | Path],
                                    TimeLeft - Dist,
                                    Unopened,
                                    Map,
                                    Distances,
                                    Current,
                                    Acc
                                )
                            )
                        end,
                        Best,
                        Nexts
                    )
            end
    end.

part1() ->
    S = input2(),
    % valve -> {flow, tunnels}
    Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
    Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
    Distances = lists:foldl(
        fun(V, D) -> distance(V, V, 0, Map, D) end,
        maps:new(),
        ["AA"] ++ Unopened
    ),
    erlang:display(best(["AA"], 30, ordsets:from_list(Unopened), Map, Distances, 0, 0)).

open(Pos, TimeLeft, Unopened, Map, Current) ->
    Score = Current + getFlow(Pos, Map) * (TimeLeft - 1),
    {Score, ordsets:del_element(Pos, Unopened)}.
sorted(List, Map) ->
    lists:sort(
        fun(A, B) ->
            getFlow(A, Map) =< getFlow(B, Map)
        end,
        List
    ).

best2(_, _, T, _, _, _, Current, _) when T =< 0 ->
    Current;
best2(_, _, _, [], _, _, Current, _) ->
    Current;
% full paths aren't strictly necessary (we only need the current position)
% but I kept track of them for debugging purposes
best2(
    {[HPos | _] = HPath, HT},
    {[EPos | _] = EPath, ET},
    TimeLeft,
    Unopened,
    Map,
    Distances,
    Current,
    Best
) ->
    if
        % heuristic: end early if tree looks bad
        TimeLeft < 16 andalso Current < (Best div 2) ->
            Current;
        true ->
            case {HT, ET, lists:member(HPos, Unopened), lists:member(EPos, Unopened)} of
                % next for both
                {0, 0, false, false} ->
                    Nexts = sorted(Unopened, Map),
                    lists:foldl(
                        fun(HNext, Acc) ->
                            lists:foldl(
                                fun(ENext, Acc2) ->
                                    if
                                        HNext == ENext ->
                                            Acc2;
                                        HPos /= EPos andalso HNext > ENext ->
                                            Acc2;
                                        true ->
                                            HDist = maps:get({HPos, HNext}, Distances),
                                            EDist = maps:get({EPos, ENext}, Distances),
                                            NextT = min(HDist, EDist),
                                            max(
                                                Acc2,
                                                best2(
                                                    {[HNext | HPath], HDist - NextT},
                                                    {[ENext | EPath], EDist - NextT},
                                                    TimeLeft - NextT,
                                                    Unopened,
                                                    Map,
                                                    Distances,
                                                    Current,
                                                    Acc2
                                                )
                                            )
                                    end
                                end,
                                Acc,
                                Nexts
                            )
                        end,
                        Best,
                        Nexts
                    );
                % open for human, next for elephant
                {0, 0, true, false} ->
                    {Score, Rem} = open(HPos, TimeLeft, Unopened, Map, Current),
                    Nexts = sorted(Rem, Map),
                    lists:foldl(
                        fun(Next, Acc) ->
                            Dist = maps:get({EPos, Next}, Distances),
                            max(
                                Acc,
                                best2(
                                    {[HPos, TimeLeft - 1 | HPath], 0},
                                    {[Next | EPath], Dist - 1},
                                    TimeLeft - 1,
                                    Rem,
                                    Map,
                                    Distances,
                                    Score,
                                    Acc
                                )
                            )
                        end,
                        Best,
                        Nexts
                    );
                % open for elephant, next for human
                {0, 0, false, true} ->
                    {Score, Rem} = open(EPos, TimeLeft, Unopened, Map, Current),
                    Nexts = sorted(Rem, Map),
                    lists:foldl(
                        fun(Next, Acc) ->
                            Dist = maps:get({HPos, Next}, Distances),
                            max(
                                Acc,
                                best2(
                                    {[Next | HPath], Dist - 1},
                                    {[EPos, TimeLeft - 1 | EPath], 0},
                                    TimeLeft - 1,
                                    Rem,
                                    Map,
                                    Distances,
                                    Score,
                                    Acc
                                )
                            )
                        end,
                        Best,
                        Nexts
                    );
                % open for both
                {0, 0, true, true} ->
                    {Score, Rem} = open(EPos, TimeLeft, Unopened, Map, Current),
                    {Score2, Rem2} = open(HPos, TimeLeft, Rem, Map, Score),
                    % there exists a condition where both parties can arrive at the same
                    % node at the same time
                    Score3 =
                        case HPos == EPos of
                            true -> Score;
                            false -> Score2
                        end,
                    best2(
                        {[HPos, TimeLeft - 1 | HPath], 0},
                        {[EPos, TimeLeft - 1 | EPath], 0},
                        TimeLeft - 1,
                        Rem2,
                        Map,
                        Distances,
                        Score3,
                        max(Best, Score3)
                    );
                % open for human, nothing for elephant
                {0, _, true, _} ->
                    {Score, Rem} = open(HPos, TimeLeft, Unopened, Map, Current),
                    best2(
                        {[HPos, TimeLeft - 1 | HPath], 0},
                        {EPath, ET - 1},
                        TimeLeft - 1,
                        Rem,
                        Map,
                        Distances,
                        Score,
                        max(Best, Score)
                    );
                % next for human, nothing for elephant
                {0, _, false, _} ->
                    Nexts = ordsets:del_element(EPos, sorted(Unopened, Map)),
                    lists:foldl(
                        fun(Next, Acc) ->
                            Dist = maps:get({HPos, Next}, Distances),
                            NextT = min(ET, Dist),
                            max(
                                Acc,
                                best2(
                                    {[Next | HPath], Dist - NextT},
                                    {EPath, ET - NextT},
                                    TimeLeft - NextT,
                                    Unopened,
                                    Map,
                                    Distances,
                                    Current,
                                    Acc
                                )
                            )
                        end,
                        Best,
                        Nexts
                    );
                % open for elephant, nothing for human
                {_, 0, _, true} ->
                    {Score, Rem} = open(EPos, TimeLeft, Unopened, Map, Current),
                    best2(
                        {HPath, HT - 1},
                        {[EPos, TimeLeft - 1 | EPath], 0},
                        TimeLeft - 1,
                        Rem,
                        Map,
                        Distances,
                        Score,
                        max(Best, Score)
                    );
                % next for elephant, nothing for human
                {_, 0, _, false} ->
                    Nexts = ordsets:del_element(HPos, sorted(Unopened, Map)),
                    lists:foldl(
                        fun(Next, Acc) ->
                            Dist = maps:get({EPos, Next}, Distances),
                            NextT = min(HT, Dist),
                            max(
                                Acc,
                                best2(
                                    {HPath, HT - NextT},
                                    {[Next | EPath], Dist - NextT},
                                    TimeLeft - NextT,
                                    Unopened,
                                    Map,
                                    Distances,
                                    Current,
                                    Acc
                                )
                            )
                        end,
                        Best,
                        Nexts
                    )
            end
    end.

part2() ->
    S = input(),
    % valve -> {flow, tunnels}
    Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
    Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
    Distances = lists:foldl(
        fun(V, D) -> distance(V, V, 0, Map, D) end,
        maps:new(),
        ["AA"] ++ Unopened
    ),
    erlang:display(
        best2({["AA"], 0}, {["AA"], 0}, 26, ordsets:from_list(Unopened), Map, Distances, 0, 0)
    ).
