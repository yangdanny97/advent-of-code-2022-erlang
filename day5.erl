-module(day5).
-compile(export_all).

parseInstruction(S) ->
    case string:tokens(S, " ") of
        [_, N1, _, N2, _, N3] -> {list_to_integer(N1), list_to_integer(N2), list_to_integer(N3)};
        _ -> []
    end.

move(Boxes, 0, _, _) ->
    Boxes;
move(Boxes, N, From, To) ->
    case element(From, Boxes) of
        [H | T] ->
            Boxes2 = setelement(From, Boxes, T),
            Boxes3 = setelement(To, Boxes2, [H | element(To, Boxes2)]),
            move(Boxes3, N - 1, From, To);
        _ ->
            Boxes
    end.

part1() ->
    B = {
        "NZ",
        "DCM",
        "P"
    },
    InitBoxes = list_to_tuple(lists:map(fun(Stack) -> [[X] || X <- Stack] end, tuple_to_list(B))),
    S =
        "\n"
        "move 1 from 2 to 1\n"
        "move 3 from 1 to 3\n"
        "move 2 from 2 to 1\n"
        "move 1 from 1 to 2",
    Instructions = [
        parseInstruction(X)
     || X <- string:split(S ++ "\n", "\n", all), not string:equal(X, "")
    ],
    Boxes = lists:foldl(
        fun({N, From, To}, Acc) ->
            move(Acc, N, From, To)
        end,
        InitBoxes,
        Instructions
    ),
    Top = lists:flatten([hd(X) || X <- tuple_to_list(Boxes)]),
    erlang:display(Top).

move2(Boxes, N, From, To) ->
    FromStack = element(From, Boxes),
    {Moving, NewFrom} = {lists:sublist(FromStack, N), lists:nthtail(N, FromStack)},
    Boxes2 = setelement(From, Boxes, NewFrom),
    setelement(To, Boxes2, lists:append(Moving, element(To, Boxes2))).

part2() ->
    B = {
        "NZ",
        "DCM",
        "P"
    },
    InitBoxes = list_to_tuple(lists:map(fun(Stack) -> [[X] || X <- Stack] end, tuple_to_list(B))),
    S =
        "\n"
        "move 1 from 2 to 1\n"
        "move 3 from 1 to 3\n"
        "move 2 from 2 to 1\n"
        "move 1 from 1 to 2",
    Instructions = [
        parseInstruction(X)
     || X <- string:split(S ++ "\n", "\n", all), not string:equal(X, "")
    ],
    Boxes = lists:foldl(
        fun({N, From, To}, Acc) ->
            move2(Acc, N, From, To)
        end,
        InitBoxes,
        Instructions
    ),
    Top = lists:flatten([hd(X) || X <- tuple_to_list(Boxes)]),
    erlang:display(Top).
