-module(day11).
-compile(export_all).

-record(monkey, {
    number,
    starting,
    operation,
    test,
    ift,
    iff
}).

input() ->
    [
        #monkey{
            number = 0,
            starting = [79, 98],
            operation = fun(X) -> X * 19 end,
            test = 23,
            ift = 2,
            iff = 3
        },
        #monkey{
            number = 1,
            starting = [54, 65, 75, 74],
            operation = fun(X) -> X + 6 end,
            test = 19,
            ift = 2,
            iff = 0
        },
        #monkey{
            number = 2,
            starting = [79, 60, 97],
            operation = fun(X) -> X * X end,
            test = 13,
            ift = 1,
            iff = 3
        },
        #monkey{
            number = 3,
            starting = [74],
            operation = fun(X) -> X + 3 end,
            test = 17,
            ift = 0,
            iff = 1
        }
    ].

inputFull() ->
    [
        #monkey{
            number = 0,
            starting = [64],
            operation = fun(X) -> X * 7 end,
            test = 13,
            ift = 1,
            iff = 3
        },
        #monkey{
            number = 1,
            starting = [60, 84, 84, 65],
            operation = fun(X) -> X + 7 end,
            test = 19,
            ift = 2,
            iff = 7
        },
        #monkey{
            number = 2,
            starting = [52, 67, 74, 88, 51, 61],
            operation = fun(X) -> X * 3 end,
            test = 5,
            ift = 5,
            iff = 7
        },
        #monkey{
            number = 3,
            starting = [67, 72],
            operation = fun(X) -> X + 3 end,
            test = 2,
            ift = 1,
            iff = 2
        },
        #monkey{
            number = 4,
            starting = [80, 79, 58, 77, 68, 74, 98, 64],
            operation = fun(X) -> X * X end,
            test = 17,
            ift = 6,
            iff = 0
        },
        #monkey{
            number = 5,
            starting = [62, 53, 61, 89, 86],
            operation = fun(X) -> X + 8 end,
            test = 11,
            ift = 4,
            iff = 6
        },
        #monkey{
            number = 6,
            starting = [86, 89, 82],
            operation = fun(X) -> X + 2 end,
            test = 7,
            ift = 3,
            iff = 0
        },
        #monkey{
            number = 7,
            starting = [92, 81, 70, 96, 69, 84, 83],
            operation = fun(X) -> X + 4 end,
            test = 3,
            ift = 4,
            iff = 5
        }
    ].

processMonkey(Monkey, Map, Adjust) ->
    N = Monkey#monkey.number,
    {Items, Count} = maps:get(N, Map),
    UpdatedMap = queue:fold(
        fun(Item, Acc) ->
            Op = Monkey#monkey.operation,
            Inspected = Op(Item),
            Adjusted = Adjust(Inspected),
            Transfer =
                case (Adjusted rem Monkey#monkey.test) == 0 of
                    true -> Monkey#monkey.ift;
                    false -> Monkey#monkey.iff
                end,
            {Q, C} = maps:get(Transfer, Acc),
            maps:put(Transfer, {queue:in(Adjusted, Q), C}, Acc)
        end,
        Map,
        Items
    ),
    NewEntry = {queue:new(), Count + queue:len(Items)},
    maps:put(N, NewEntry, UpdatedMap).

processMonkey1(Monkey, Map) ->
    processMonkey(Monkey, Map, fun(X) -> X div 3 end).

part1() ->
    Monkeys = inputFull(),
    % monkey no => {item queue, count}
    Init = maps:from_list(
        lists:map(fun(M) -> {M#monkey.number, {queue:from_list(M#monkey.starting), 0}} end, Monkeys)
    ),
    Result = lists:foldl(
        fun(_, Acc) -> lists:foldl(fun processMonkey1/2, Acc, Monkeys) end, Init, lists:seq(1, 20)
    ),
    [N1, N2 | _] = lists:reverse(
        lists:sort(lists:map(fun({_, X}) -> X end, maps:values(Result)))
    ),
    erlang:display(N1 * N2).

part2() ->
    Monkeys = inputFull(),
    Init = maps:from_list(
        lists:map(fun(M) -> {M#monkey.number, {queue:from_list(M#monkey.starting), 0}} end, Monkeys)
    ),
    % get product of all the numbers we test against
    % instead of worry div 3, use worry rem TestProduct
    TestProduct = lists:foldl(
        fun(X, Acc) -> X * Acc end, 1, lists:map(fun(M) -> M#monkey.test end, Monkeys)
    ),
    Result = lists:foldl(
        fun(_, Acc) ->
            lists:foldl(
                fun(M, A) -> processMonkey(M, A, fun(X) -> X rem TestProduct end) end,
                Acc,
                Monkeys
            )
        end,
        Init,
        lists:seq(1, 10000)
    ),
    [N1, N2 | _] = lists:reverse(
        lists:sort(lists:map(fun({_, X}) -> X end, maps:values(Result)))
    ),
    erlang:display(N1 * N2).
