-module(day1).
-compile(export_all).

part1()->
    S = "
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
",
    Split = string:split(S ++ "\n", "\n", all),
    {Max, _} = lists:foldl(
        fun(X, {Max, Curr}) -> 
            case string:equal(X, "") of
                true -> {Max, 0};
                false -> 
                    XInt = list_to_integer(X),
                    Curr2 = Curr + XInt,
                    {max(Curr2, Max), Curr2}
            end
        end,
        {0, 0},
        Split
    ),
    io:format("~w~n", [Max]).

part2()->
    S = "
1000
2000
3000

4000

5000
6000

7000
8000
9000

10000
",
    Split = string:split(S ++ "\n", "\n", all),
    ListSum = fun(L) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) end,
    {Sums, _} = lists:foldl(
        fun(X, {Sums, Curr}) -> 
            case string:equal(X, "") of
                true -> {[ListSum(Curr) | Sums], []};
                false -> 
                    XInt = list_to_integer(X),
                    {Sums, [XInt | Curr]}
            end
        end,
        {[], []},
        Split
    ),
    case lists:reverse(lists:sort(Sums)) of
        [N1, N2, N3 | _] -> io:format("~w~n", [N1 + N2 + N3]);
        [_] -> io:format("~w~n", [0])
    end.