-module(day1).
-compile(export_all).

part1() ->
    S =
        "1000\n"
        "2000\n"
        "3000\n"
        "\n"
        "4000\n"
        "\n"
        "5000\n"
        "6000\n"
        "\n"
        "7000\n"
        "8000\n"
        "9000\n"
        "\n"
        "10000\n",
    Split = string:split(S ++ "\n", "\n", all),
    {Max, _} = lists:foldl(
        fun(X, {Max, Curr}) ->
            case string:equal(X, "") of
                true ->
                    {Max, 0};
                false ->
                    XInt = list_to_integer(X),
                    Curr2 = Curr + XInt,
                    {max(Curr2, Max), Curr2}
            end
        end,
        {0, 0},
        Split
    ),
    erlang:display(Max).

part2() ->
    S =
        "1000\n"
        "2000\n"
        "3000\n"
        "\n"
        "4000\n"
        "\n"
        "5000\n"
        "6000\n"
        "\n"
        "7000\n"
        "8000\n"
        "9000\n"
        "\n"
        "10000\n",
    Split = string:split(S ++ "\n", "\n", all),
    ListSum = fun(L) -> lists:foldl(fun(X, Sum) -> X + Sum end, 0, L) end,
    {Sums, _} = lists:foldl(
        fun(X, {Sums, Curr}) ->
            case string:equal(X, "") of
                true ->
                    {[ListSum(Curr) | Sums], []};
                false ->
                    XInt = list_to_integer(X),
                    {Sums, [XInt | Curr]}
            end
        end,
        {[], []},
        Split
    ),
    case lists:reverse(lists:sort(Sums)) of
        [N1, N2, N3 | _] -> erlang:display(N1 + N2 + N3)
    end.
