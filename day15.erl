-module(day15).
-compile(export_all).

% this one is pretty inefficient and takes a while to run

input() ->
    "Sensor at x=2, y=18: closest beacon is at x=-2, y=15\n"
    "Sensor at x=9, y=16: closest beacon is at x=10, y=16\n"
    "Sensor at x=13, y=2: closest beacon is at x=15, y=3\n"
    "Sensor at x=12, y=14: closest beacon is at x=10, y=16\n"
    "Sensor at x=10, y=20: closest beacon is at x=10, y=16\n"
    "Sensor at x=14, y=17: closest beacon is at x=10, y=16\n"
    "Sensor at x=8, y=7: closest beacon is at x=2, y=10\n"
    "Sensor at x=2, y=0: closest beacon is at x=2, y=10\n"
    "Sensor at x=0, y=11: closest beacon is at x=2, y=10\n"
    "Sensor at x=20, y=14: closest beacon is at x=25, y=17\n"
    "Sensor at x=17, y=20: closest beacon is at x=21, y=22\n"
    "Sensor at x=16, y=7: closest beacon is at x=15, y=3\n"
    "Sensor at x=14, y=3: closest beacon is at x=15, y=3\n"
    "Sensor at x=20, y=1: closest beacon is at x=15, y=3".

processLine(Line) ->
    S = re:replace(Line, "Sensor at x=", "", [global, {return, list}]),
    S2 = re:replace(S, " y=", "", [global, {return, list}]),
    S3 = re:replace(S2, ": closest beacon is at x=", ",", [global, {return, list}]),
    list_to_tuple([list_to_integer(X) || X <- string:tokens(S3, ",")]).

dist({Sx, Sy}, {Bx, By}) ->
    abs(Sx - Bx) + abs(Sy - By).

part1() ->
    S = input(),
    Y = 10,
    % list of {sensorX, sensorY, beaconX, beaconY}
    Points = [processLine(Line) || Line <- string:tokens(S, "\n")],
    % get all beacons at y=Y
    Beacons = lists:foldl(
        fun({_, _, Bx, By}, Acc) ->
            case By == Y of
                true -> ordsets:add_element(Bx, Acc);
                false -> Acc
            end
        end,
        ordsets:new(),
        Points
    ),
    % get set of all points within sensor range at y=Y
    Sensors =
        lists:foldl(
            fun({Sx, Sy, Bx, By}, Acc) ->
                D = dist({Sx, Sy}, {Bx, By}),
                if
                    (Y =< (Sy + D)) and ((Sy - D) =< Y) ->
                        Offset = D - abs(Sy - Y),
                        ordsets:union(lists:seq(Sx - Offset, Sx + Offset), Acc);
                    true ->
                        Acc
                end
            end,
            ordsets:new(),
            Points
        ),
    erlang:display(ordsets:size(ordsets:subtract(Sensors, Beacons))).

% points in a diamond perimiter of distance D around point X,Y that are within the search area
perimeter({X, Y}, D, Max) ->
    UL = lists:zip(lists:seq(X - D, X - 1), lists:reverse(lists:seq(Y - D + 1, Y))),
    UR = lists:zip(lists:seq(X, X + D - 1), lists:seq(Y - D, Y - 1)),
    DL = lists:zip(lists:seq(X - D + 1, X), lists:seq(Y + 1, Y + D)),
    DR = lists:zip(lists:seq(X + 1, X + D), lists:reverse(lists:seq(Y, Y + D - 1))),
    ordsets:from_list(
        lists:filter(
            fun({Px, Py}) -> (Px >= 0) and (Px =< Max) and (Py >= 0) and (Py =< Max) end,
            UL ++ UR ++ DL ++ DR
        )
    ).

part2() ->
    S = input(),
    Max = 20,
    % list of {sensorX, sensorY, beaconX, beaconY}
    Points = [processLine(Line) || Line <- string:tokens(S, "\n")],
    % new beacon must be:
    % 1. just outside the perimeter of some sensor
    % 2. outside the perimeter of all sensors
    Search =
        lists:foldl(
            fun({Sx, Sy, Bx, By}, Acc) ->
                D = dist({Sx, Sy}, {Bx, By}),
                ordsets:union(perimeter({Sx, Sy}, D + 1, Max), Acc)
            end,
            ordsets:new(),
            Points
        ),
    EmptySpots = lists:filter(
        fun({X, Y}) ->
            not lists:any(
                fun({Sx, Sy, Bx, By}) ->
                    D = dist({Sx, Sy}, {Bx, By}),
                    D2 = dist({Sx, Sy}, {X, Y}),
                    D2 =< D
                end,
                Points
            )
        end,
        Search
    ),
    {X, Y} = hd(EmptySpots),
    erlang:display(X * 4000000 + Y).
