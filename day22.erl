-module(day22).
-compile(export_all).

map() ->
    "        ...#\n"
    "        .#..\n"
    "        #...\n"
    "        ....\n"
    "...#.......#\n"
    "........#...\n"
    "..#....#....\n"
    "..........#.\n"
    "        ...#....\n"
    "        .....#..\n"
    "        .#......\n"
    "        ......#.".

input() -> "10R5L5R10L4R5L5".

makeMap(Input) ->
    Lines = string:tokens(Input, "\n"),
    lists:foldl(
        fun({Y, Line}, Acc) ->
            lists:foldl(
                fun({X, Char}, Acc2) ->
                    case [Char] of
                        " " -> Acc2;
                        "#" -> maps:put({X, Y}, 1, Acc2);
                        "." -> maps:put({X, Y}, 0, Acc2)
                    end
                end,
                Acc,

                lists:zip(lists:seq(1, length(Line)), Line)
            )
        end,
        maps:new(),
        lists:zip(lists:seq(1, length(Lines)), Lines)
    ).

helper([], Acc) -> {Acc, []};
helper([76 | _] = L, Acc) -> {Acc, L};
helper([82 | _] = L, Acc) -> {Acc, L};
helper([X | T], Acc) -> helper(T, [X | Acc]).

next([76 | T]) ->
    {"L", T};
next([82 | T]) ->
    {"R", T};
next(I) ->
    {Acc, Remaining} = helper(I, []),
    {list_to_integer(lists:reverse(Acc)), Remaining}.

getXs(R, Map) -> [X || {X, _} <- lists:filter(fun({_, Y}) -> Y == R end, maps:keys(Map))].
getYs(C, Map) -> [Y || {_, Y} <- lists:filter(fun({X, _}) -> X == C end, maps:keys(Map))].

wrap1(X, Y, O, Map) ->
    case O of
        % R
        0 -> {lists:min(getXs(Y, Map)), Y, O};
        % D
        1 -> {X, lists:min(getYs(X, Map)), O};
        % L
        2 -> {lists:max(getXs(Y, Map)), Y, O};
        % U
        3 -> {X, lists:max(getYs(X, Map)), O}
    end.

move(X, Y, 0, O, [], _, _) ->
    {X, Y, O};
move(X, Y, N, Orientation, Input, Map, Wrap) ->
    case N == 0 of
        false ->
            {Dx, Dy} =
                case Orientation of
                    % R
                    0 -> {1, 0};
                    % D
                    1 -> {0, 1};
                    % L
                    2 -> {-1, 0};
                    % U
                    3 -> {0, -1}
                end,
            {Nx, Ny} = {X + Dx, Y + Dy},
            case maps:get({Nx, Ny}, Map, -1) of
                % open, go
                0 ->
                    move(Nx, Ny, N - 1, Orientation, Input, Map, Wrap);
                % wall, stop
                1 ->
                    move(X, Y, 0, Orientation, Input, Map, Wrap);
                % off the map, calculate next
                -1 ->
                    {Wx, Wy, O2} = Wrap(X, Y, Orientation, Map),
                    case maps:get({Wx, Wy}, Map, -1) of
                        0 -> move(Wx, Wy, N - 1, O2, Input, Map, Wrap);
                        1 -> move(X, Y, 0, Orientation, Input, Map, Wrap)
                    end
            end;
        true ->
            case next(Input) of
                {"L", Rem} ->
                    % erlang:display("Turn L"),
                    O2 =
                        case Orientation of
                            0 -> 3;
                            O -> O - 1
                        end,
                    move(X, Y, 0, O2, Rem, Map, Wrap);
                {"R", Rem} ->
                    % erlang:display("Turn R"),
                    O2 =
                        case Orientation of
                            3 -> 0;
                            O -> O + 1
                        end,
                    move(X, Y, 0, O2, Rem, Map, Wrap);
                {Move, Rem} ->
                    % erlang:display({X, Y, "Move", Move}),
                    move(X, Y, Move, Orientation, Rem, Map, Wrap)
            end
    end.

normalize({X, Y}, N) -> {1 + ((X - 1) rem N), 1 + ((Y - 1) rem N)}.
side({X, Y}, N) -> {(X - 1) div N, (Y - 1) div N}.
translate({X, Y}, {EI, EJ}, N) ->
    {X + EI * N, Y + EJ * N}.
flipX({X, Y}, N) -> {N - X + 1, Y}.
flipY({X, Y}, N) -> {X, N - Y + 1}.
rotate({X, Y}, _, 0) ->
    {X, Y};
rotate({X, Y}, N, R) when R > 0 ->
    rotate({N - Y + 1, X}, N, R - 1).

wrap2ex(X, Y, O, _) ->
    N = 4,
    C = normalize({X, Y}, N),
    S = side({X, Y}, N),
    Side =
        case S of
            {2, 0} -> 1;
            {0, 1} -> 2;
            {1, 1} -> 3;
            {2, 1} -> 4;
            {2, 2} -> 5;
            {3, 2} -> 6
        end,
    case {Side, O} of
        % 1R -> 6L
        {1, 0} ->
            E = {3, 2},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 2};
        % 6R -> 1L
        {6, 0} ->
            E = {2, 0},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 2};
        % 4R -> 6D
        {4, 0} ->
            E = {3, 2},
            C2 = rotate(C, N, 1),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 1};
        % 6U -> 4L
        {6, 3} ->
            E = {2, 1},
            C2 = rotate(C, N, 1),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 2};
        % 2D -> 5U
        {2, 1} ->
            E = {2, 2},
            C2 = flipX(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 3};
        % 5D -> 2U
        {5, 1} ->
            E = {0, 1},
            C2 = flipX(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 3};
        % 3D -> 5R
        {3, 1} ->
            E = {2, 2},
            C2 = rotate(C, N, 1),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 0};
        % 5L -> 3U
        {5, 2} ->
            E = {1, 1},
            C2 = rotate(C, N, 1),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 3};
        % 6D -> 2R
        {6, 1} ->
            E = {0, 1},
            C2 = flipX(C, N),
            C3 = rotate(C2, N, 1),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 0};
        % 2L -> 6U
        {2, 2} ->
            E = {3, 2},
            C2 = rotate(C, N, 3),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 3};
        % 1L -> 3D
        {1, 2} ->
            E = {1, 1},
            C2 = rotate(C, N, 1),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 1};
        % 3U -> 1R
        {3, 3} ->
            E = {2, 0},
            C2 = rotate(C, N, 3),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 0};
        % 2U -> 1D
        {2, 3} ->
            E = {2, 0},
            C2 = flipX(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 1};
        % 1U -> 2D
        {1, 3} ->
            E = {0, 1},
            C2 = flipX(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 1}
    end.

wrap2full(X, Y, O, _) ->
    N = 50,
    C = normalize({X, Y}, N),
    S = side({X, Y}, N),
    Side =
        case S of
            {1, 0} -> 1;
            {2, 0} -> 2;
            {1, 1} -> 3;
            {0, 2} -> 4;
            {1, 2} -> 5;
            {0, 3} -> 6
        end,
    case {Side, O} of
        % 2U 6U
        {2, 3} ->
            E = {0, 3},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 3};
        % 6D 2D
        {6, 1} ->
            E = {2, 0},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 1};
        % 2D 3L
        {2, 1} ->
            E = {1, 1},
            C2 = rotate(C, N, 3),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 2};
        % 3R 2U
        {3, 0} ->
            E = {2, 0},
            C2 = rotate(C, N, 1),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 3};
        % 2R 5L
        {2, 0} ->
            E = {1, 2},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 2};
        % 5R 2L
        {5, 0} ->
            E = {2, 0},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 2};
        % 4U 3R
        {4, 3} ->
            E = {1, 1},
            C2 = rotate(C, N, 3),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 0};
        % 3L 4D
        {3, 2} ->
            E = {0, 2},
            C2 = rotate(C, N, 1),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 1};
        % 6R 5U
        {6, 0} ->
            E = {1, 2},
            C2 = rotate(C, N, 1),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 3};
        % 5D 6L
        {5, 1} ->
            E = {0, 3},
            C2 = rotate(C, N, 3),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 2};
        % 4L 1R
        {4, 2} ->
            E = {1, 0},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 0};
        % 1L 4R
        {1, 2} ->
            E = {0, 2},
            C2 = flipY(C, N),
            {X2, Y2} = translate(C2, E, N),
            {X2, Y2, 0};
        % 6L 1D
        {6, 2} ->
            E = {1, 0},
            C2 = rotate(C, N, 1),
            C3 = flipX(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 1};
        % 1U 6R
        {1, 3} ->
            E = {0, 3},
            C2 = rotate(C, N, 3),
            C3 = flipY(C2, N),
            {X2, Y2} = translate(C3, E, N),
            {X2, Y2, 0}
    end.

part1() ->
    Map = makeMap(map()),
    {Sx, Sy} = {lists:min(getXs(1, Map)), 1},
    {X, Y, O} = move(Sx, Sy, 0, 0, input(), Map, fun wrap1/4),
    erlang:display(Y * 1000 + X * 4 + O).

part2() ->
    {M, I, W} = {map(), input(), fun wrap2ex/4},
    Map = makeMap(M),
    {Sx, Sy} = {lists:min(getXs(1, Map)), 1},
    {X, Y, O} = move(Sx, Sy, 0, 0, I, Map, W),
    erlang:display(Y * 1000 + X * 4 + O).
