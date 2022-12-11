-module(day5).
-compile(export_all).

input() ->
    {
        "move 1 from 2 to 1\n"
        "move 3 from 1 to 3\n"
        "move 2 from 2 to 1\n"
        "move 1 from 1 to 2",
        {
            "NZ",
            "DCM",
            "P"
        }
    }.

parseInstruction(S) ->
    case string:tokens(S, " ") of
        [_, N1, _, N2, _, N3] -> {list_to_integer(N1), list_to_integer(N2), list_to_integer(N3)};
        _ -> []
    end.

move1({0, _, _}, Boxes) ->
    Boxes;
move1({N, From, To}, Boxes) ->
    case element(From, Boxes) of
        [H | T] ->
            Boxes2 = setelement(From, Boxes, T),
            Boxes3 = setelement(To, Boxes2, [H | element(To, Boxes2)]),
            move1({N - 1, From, To}, Boxes3);
        _ ->
            Boxes
    end.

part1() ->
    % first char in string = top of stack
    {B, S} = input(),
    InitBoxes = list_to_tuple(lists:map(fun(Stack) -> [[X] || X <- Stack] end, tuple_to_list(B))),
    Instructions = [parseInstruction(X) || X <- string:tokens(S, "\n")],
    Boxes = lists:foldl(fun move1/2, InitBoxes, Instructions),
    Top = lists:flatten([hd(X) || X <- tuple_to_list(Boxes)]),
    erlang:display(Top).

move2({N, From, To}, Boxes) ->
    FromStack = element(From, Boxes),
    {Moving, NewFrom} = {lists:sublist(FromStack, N), lists:nthtail(N, FromStack)},
    Boxes2 = setelement(From, Boxes, NewFrom),
    setelement(To, Boxes2, lists:append(Moving, element(To, Boxes2))).

part2() ->
    % first char in string = top of stack
    {B, S} = input(),
    InitBoxes = list_to_tuple(lists:map(fun(Stack) -> [[X] || X <- Stack] end, tuple_to_list(B))),
    Instructions = [parseInstruction(X) || X <- string:tokens(S, "\n")],
    Boxes = lists:foldl(fun move2/2, InitBoxes, Instructions),
    Top = lists:flatten([hd(X) || X <- tuple_to_list(Boxes)]),
    erlang:display(Top).
