-module(day8).
-compile(export_all).

input() ->
    "30373\n"
    "25512\n"
    "65332\n"
    "33549\n"
    "35390".

check(Map, X, Y, CurrH, Dx, Dy) ->
    case maps:get({X, Y}, Map, -1) of
        -1 -> [];
        H when H > CurrH -> [{X, Y} | check(Map, X + Dx, Y + Dy, H, Dx, Dy)];
        _ -> check(Map, X + Dx, Y + Dy, CurrH, Dx, Dy)
    end.

part1() ->
    S = input(),
    Lines = string:tokens(S, "\n"),
    % {X, Y} => Height
    Map = maps:from_list(
        lists:flatten([
            [
                {{X, Y}, list_to_integer([Val])}
             || {X, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
            ]
         || {Y, Row} <- lists:zip(lists:seq(1, length(Lines)), Lines)
        ])
    ),
    Ys = lists:seq(1, length(Lines)),
    Left = lists:flatten([check(Map, 1, Y, -1, 1, 0) || Y <- Ys]),
    Right = lists:flatten([check(Map, length(hd(Lines)), Y, -1, -1, 0) || Y <- Ys]),
    Xs = lists:seq(1, length(hd(Lines))),
    Up = lists:flatten([check(Map, X, 1, -1, 0, 1) || X <- Xs]),
    Down = lists:flatten([check(Map, X, length(Lines), -1, 0, -1) || X <- Xs]),
    All = ordsets:from_list(lists:append([Up, Down, Left, Right])),
    erlang:display(length(All)).

search(Map, X, Y, TreeH, Dx, Dy) ->
    case maps:get({X, Y}, Map, -1) of
        % edge counts as 0
        -1 -> 0;
        % count tree of same or greater height, stop searching
        H when H >= TreeH -> 1;
        _ -> 1 + search(Map, X + Dx, Y + Dy, TreeH, Dx, Dy)
    end.

part2() ->
    S = input(),
    Lines = string:tokens(S, "\n"),
    % {X, Y} => Height
    Map = maps:from_list(
        lists:flatten([
            [
                {{X, Y}, list_to_integer([Val])}
             || {X, Val} <- lists:zip(lists:seq(1, length(Row)), Row)
            ]
         || {Y, Row} <- lists:zip(lists:seq(1, length(Lines)), Lines)
        ])
    ),
    Scores = maps:values(
        maps:map(
            fun({X, Y}, H) ->
                search(Map, X + 1, Y, H, 1, 0) *
                    search(Map, X, Y + 1, H, 0, 1) *
                    search(Map, X - 1, Y, H, -1, 0) *
                    search(Map, X, Y - 1, H, 0, -1)
            end,
            Map
        )
    ),
    erlang:display(lists:max(Scores)).
