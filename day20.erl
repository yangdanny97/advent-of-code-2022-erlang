-module(day20).
-compile(export_all).

input() ->
    "1\n"
    "2\n"
    "-3\n"
    "3\n"
    "-2\n"
    "0\n"
    "4".

idx([], _, _) ->
    not_found;
idx([Hd | Tl], El, N) ->
    case Hd == El of
        true -> N;
        false -> idx(Tl, El, N + 1)
    end.
idx(List, El) ->
    idx(List, El, 1).

nextPos(Start, 0, _) ->
    Start;
nextPos(Start, Change, Length) ->
    case (Start - 1 + Change) rem (Length - 1) of
        X when X < 0 -> X + Length - 1;
        X -> X
    end + 1.

mix(Items, Ordered, Length) ->
    lists:foldl(
        fun({_, Num} = Item, Acc) ->
            Pos = idx(Acc, Item),
            NextPos = nextPos(Pos, Num, Length),
            Acc2 =
                if
                    NextPos == Pos ->
                        Acc;
                    NextPos < Pos ->
                        lists:sublist(Acc, NextPos - 1) ++ [Item] ++
                            lists:sublist(Acc, NextPos, Pos - NextPos) ++
                            lists:sublist(Acc, Pos + 1, Length);
                    NextPos > Pos ->
                        lists:sublist(Acc, Pos - 1) ++ lists:sublist(Acc, Pos + 1, NextPos - Pos) ++
                            [Item] ++ lists:sublist(Acc, NextPos + 1, Length)
                end,
            Acc2
        end,
        Items,
        Ordered
    ).

results(Numbers) ->
    Zero = idx(Numbers, 0),
    lists:sum(
        lists:map(
            fun(N) ->
                Idx = ((Zero - 1 + N) rem length(Numbers)) + 1,
                lists:nth(Idx, Numbers)
            end,
            [1000, 2000, 3000]
        )
    ).

part1() ->
    Numbers = [list_to_integer(N) || N <- string:tokens(input(), "\n")],
    Length = length(Numbers),
    Seq = lists:seq(1, Length),
    Init = lists:zip(Seq, Numbers),
    Mixed = mix(Init, Init, Length),
    erlang:display(results(lists:map(fun({_, X}) -> X end, Mixed))).

part2() ->
    Initial = [list_to_integer(N) || N <- string:tokens(input(), "\n")],
    Numbers = lists:map(fun(N) -> N * 811589153 end, Initial),
    Length = length(Numbers),
    Seq = lists:seq(1, Length),
    Mixed = lists:foldl(
        fun(_, Prev) ->
            mix(Prev, lists:sort(fun({X, _}, {Y, _}) -> X =< Y end, Prev), Length)
        end,
        lists:zip(Seq, Numbers),
        lists:seq(1, 10)
    ),
    erlang:display(results(lists:map(fun({_, X}) -> X end, Mixed))).
