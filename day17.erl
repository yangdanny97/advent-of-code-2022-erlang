-module(day17).
-compile(export_all).

input() -> ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>".

rock(R, Y) ->
    case R rem 5 of
        1 -> ordsets:from_list([{2, Y + 4}, {3, Y + 4}, {4, Y + 4}, {5, Y + 4}]);
        2 -> ordsets:from_list([{2, Y + 5}, {3, Y + 5}, {4, Y + 5}, {3, Y + 4}, {3, Y + 6}]);
        3 -> ordsets:from_list([{2, Y + 4}, {3, Y + 4}, {4, Y + 4}, {4, Y + 5}, {4, Y + 6}]);
        4 -> ordsets:from_list([{2, Y + 4}, {2, Y + 5}, {2, Y + 6}, {2, Y + 7}]);
        0 -> ordsets:from_list([{2, Y + 4}, {2, Y + 5}, {3, Y + 4}, {3, Y + 5}])
    end.

getY(Rocks) ->
    lists:map(fun({_, Y}) -> Y end, Rocks).

getX(Rocks) ->
    lists:map(fun({X, _}) -> X end, Rocks).

jet([], Rock, Rocks) ->
    jet(input(), Rock, Rocks);
jet(Jets, Rock, Rocks) ->
    Rock2 =
        case hd(Jets) of
            60 -> lists:map(fun({X, Y}) -> {X - 1, Y} end, Rock);
            62 -> lists:map(fun({X, Y}) -> {X + 1, Y} end, Rock)
        end,
    MinX = lists:min(getX(Rock2)),
    MaxX = lists:max(getX(Rock2)),
    case (MinX >= 0) andalso (MaxX < 7) andalso ordsets:is_disjoint(Rock2, Rocks) of
        true -> drop(tl(Jets), Rock2, Rocks);
        false -> drop(tl(Jets), Rock, Rocks)
    end.
drop(Jets, Rock, Rocks) ->
    Rock2 = lists:map(fun({X, Y}) -> {X, Y - 1} end, Rock),
    case ordsets:is_disjoint(Rock2, Rocks) of
        true ->
            jet(Jets, Rock2, Rocks);
        false ->
            U = ordsets:union(Rock, Rocks),
            MaxY = lists:max(getY(U)),
            % filter out rocks that are too low for performance
            Rocks2 = lists:filter(fun({_, Y}) -> Y > (MaxY - 150) end, U),
            {Jets, Rocks2}
    end.

display(Rocks) ->
    MinY = lists:min(getY(Rocks)),
    MaxY = lists:max(getY(Rocks)),
    S = lists:flatten(
        lists:foldl(
            fun(R, Acc) ->
                Row = lists:filter(fun({_, Y}) -> Y == R end, Rocks),
                Dis =
                    lists:map(
                        fun(C) ->
                            case lists:keyfind(C, 1, Row) of
                                false -> 46;
                                _ -> 35
                            end
                        end,
                        lists:seq(0, 6)
                    ) ++ integer_to_list(R),
                [Dis, "\n" | Acc]
            end,
            [],
            lists:seq(MinY, MaxY)
        )
    ),
    io:format("~s", [S]).

part1() ->
    {_, Result} = lists:foldl(
        fun(R, {Jets, Rocks}) ->
            MaxY = lists:max(getY(Rocks)),
            Rock = rock(R, MaxY),
            jet(Jets, Rock, Rocks)
        end,
        {input(), ordsets:from_list([{0, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}])},
        lists:seq(1, 2022)
    ),
    erlang:display(lists:max(getY(Result))).

pattern(R, Deltas, Jets, Rocks) ->
    MaxY = lists:max(getY(Rocks)),
    Rock = rock(R, MaxY),
    {Jets2, Rocks2} = jet(Jets, Rock, Rocks),
    Delta = lists:max(getY(Rocks2)) - MaxY,
    % sanity check to measure progress
    % if
    %     (R rem 1000) == 0 -> erlang:display(R);
    %     true -> ok
    % end,
    if
        % sanity check to stop if input is too big
        R > 6000 -> not_ok;
        R > 50 -> patternHelper(R + 1, [Delta | Deltas], Jets2, Rocks2);
        true -> pattern(R + 1, [Delta | Deltas], Jets2, Rocks2)
    end.

patternHelper(R, Deltas, Jets, Rocks) ->
    Lengths = lists:seq(10, R div 2),
    Pattern = lists:foldl(
        fun(L, Acc) ->
            case lists:sublist(Deltas, L) == lists:sublist(Deltas, L + 1, L) of
                true -> {length(Deltas) - L - L, L, lists:reverse(Deltas), Rocks};
                false -> Acc
            end
        end,
        {0, 0, [], []},
        Lengths
    ),
    case Pattern of
        {S, L, _, _} when S > 10 andalso L > 10 -> Pattern;
        _ -> pattern(R, Deltas, Jets, Rocks)
    end.

part2() ->
    {S, L, Deltas, PRocks} = pattern(
        1, [], input(), ordsets:from_list([{0, 0}, {1, 0}, {2, 0}, {3, 0}, {4, 0}, {5, 0}, {6, 0}])
    ),
    Pattern = lists:sublist(Deltas, S + 1, L),
    N = length(Deltas),
    % sanity check - these should be the same
    % erlang:display({N, S + L + L}),
    RemRocks = 1000000000000 - N,
    RemPatterns = RemRocks div L,
    Rem = RemRocks rem L,
    PatternH = lists:sum(Pattern),
    H = lists:sum(Deltas),
    % sanity check - these should be the same
    % erlang:display({lists:max(getY(PRocks)), H}),
    erlang:display(H + PatternH * RemPatterns + lists:sum(lists:sublist(Pattern, Rem))).
