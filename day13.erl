-module(day13).
-compile(export_all).

input() ->
    "[1,1,3,1,1]\n"
    "[1,1,5,1,1]\n"
    "\n"
    "[[1],[2,3,4]]\n"
    "[[1],4]\n"
    "\n"
    "[9]\n"
    "[[8,7,6]]\n"
    "\n"
    "[[4,4],4,4]\n"
    "[[4,4],4,4,4]\n"
    "\n"
    "[7,7,7,7]\n"
    "[7,7,7]\n"
    "\n"
    "[]\n"
    "[3]\n"
    "\n"
    "[[[]]]\n"
    "[[]]\n"
    "\n"
    "[1,[2,[3,[4,[5,6,7]]]],8,9]\n"
    "[1,[2,[3,[4,[5,6,0]]]],8,9]".

% https://grantwinney.com/how-to-evaluate-a-string-of-code-in-erlang-at-runtime/
parse(S) ->
    {ok, Tokens, _} = erl_scan:string(S ++ "."),
    {ok, Parsed} = erl_parse:parse_term(Tokens),
    Parsed.

group([]) -> [];
group([Left, Right | T]) -> [{Left, Right} | group(T)].

compare([], []) ->
    0;
compare(_, []) ->
    -1;
compare([], _) ->
    1;
compare(Left, Right) when is_list(Left) andalso is_list(Right) ->
    case compare(hd(Left), hd(Right)) of
        1 -> 1;
        0 -> compare(tl(Left), tl(Right));
        -1 -> -1
    end;
compare(Left, Right) when not is_list(Left) andalso is_list(Right) -> compare([Left], Right);
compare(Left, Right) when not is_list(Right) andalso is_list(Left) -> compare(Left, [Right]);
compare(Left, Right) when not is_list(Left) andalso not is_list(Right) ->
    if
        Left < Right -> 1;
        Left > Right -> -1;
        true -> 0
    end.

idx(Item, List) -> idx(Item, List, 1).
idx(_, [], _) ->
    not_found;
idx(Item, [H | T], N) ->
    case H == Item of
        true -> N;
        false -> idx(Item, T, N + 1)
    end.

part1() ->
    S = input(),
    Packets = [parse(X) || X <- string:tokens(S, "\n")],
    Pairs = group(Packets),
    Ordered = [
        I
     || {I, {Left, Right}} <- lists:zip(lists:seq(1, length(Pairs)), Pairs),
        compare(Left, Right) > 0
    ],
    erlang:display(lists:sum(Ordered)).

part2() ->
    S = input(),
    P1 = [[2]],
    P2 = [[6]],
    Packets = [P1, P2] ++ [parse(X) || X <- string:tokens(S, "\n")],
    Ordered = lists:sort(fun(Left, Right) -> compare(Left, Right) >= 0 end, Packets),
    erlang:display(idx(P1, Ordered) * idx(P2, Ordered)).
