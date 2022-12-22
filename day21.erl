-module(day21).
-compile(export_all).

input() ->
    "root: pppw + sjmn\n"
    "dbpl: 5\n"
    "cczh: sllz + lgvd\n"
    "zczc: 2\n"
    "ptdq: humn - dvpt\n"
    "dvpt: 3\n"
    "lfqf: 4\n"
    "humn: 5\n"
    "ljgn: 2\n"
    "sjmn: drzm * dbpl\n"
    "sllz: 4\n"
    "pppw: cczh / lfqf\n"
    "lgvd: ljgn * ptdq\n"
    "drzm: hmdt - zczc\n"
    "hmdt: 32".

eval(Monkey, Map) ->
    case maps:get(Monkey, Map) of
        [M1, Op, M2] ->
            V1 = eval(M1, Map),
            V2 = eval(M2, Map),
            V3 =
                case Op of
                    "*" -> V1 * V2;
                    "-" -> V1 - V2;
                    "+" -> V1 + V2;
                    "/" -> V1 div V2
                end,
            V3;
        N ->
            N
    end.

add({N1, D1}, {N2, D2}) ->
    {N1 * D2 + N2 * D1, D1 * D2}.
sub({N1, D1}, {N2, D2}) ->
    {N1 * D2 - N2 * D1, D1 * D2}.
simplify({N, D}) ->
    case N rem D of
        0 -> {N div D, 1};
        _ -> {N, D}
    end.

% keep track of 2 sets of fractions: known integers and multiples of humn
eval2(Monkey, Map) ->
    {I, H} =
        case {Monkey, maps:get(Monkey, Map)} of
            {"humn", _} ->
                {{0, 1}, {1, 1}};
            {_, [M1, Op, M2]} ->
                {{N1, D1} = I1, {HN1, HD1} = H1} = V1 = eval2(M1, Map),
                {{N2, D2} = I2, {HN2, HD2} = H2} = V2 = eval2(M2, Map),
                V3 =
                    case Op of
                        "*" when HN1 == 0 -> {{N1 * N2, D1 * D2}, {N1 * HN2, HD2}};
                        "*" when HN2 == 0 -> {{N1 * N2, D1 * D2}, {N2 * HN1, HD1}};
                        "-" ->
                            {sub(I1, I2), sub(H1, H2)};
                        "+" ->
                            {add(I1, I2), add(H1, H2)};
                        "/" when HN2 == 0 andalso N2 /= 0 ->
                            {{N1 * D2, N2 * D1}, {HN1 * D2, N2 * HD1}};
                        _ ->
                            erlang:display({V1, Op, V2}),
                            not_ok
                    end,
                V3;
            {_, N} ->
                {{N, 1}, {0, 1}}
        end,
    {simplify(I), simplify(H)}.

part1() ->
    Map = lists:foldl(
        fun(L, Acc) ->
            [Monkey, E] = string:tokens(L, ":"),
            Expr =
                case string:tokens(E, " ") of
                    [N] -> list_to_integer(N);
                    X -> X
                end,
            maps:put(Monkey, Expr, Acc)
        end,
        maps:new(),
        string:tokens(input(), "\n")
    ),
    erlang:display(eval("root", Map)).

part2() ->
    Map = lists:foldl(
        fun(L, Acc) ->
            [Monkey, E] = string:tokens(L, ":"),
            Expr =
                case string:tokens(E, " ") of
                    [N] -> list_to_integer(N);
                    X -> X
                end,
            maps:put(Monkey, Expr, Acc)
        end,
        maps:new(),
        string:tokens(input(), "\n")
    ),
    [M1, _, M2] = maps:get("root", Map),
    % I1 + H1 * humn == I2 * H2 * humn
    {I1, H1} = eval2(M1, Map),
    {I2, H2} = eval2(M2, Map),
    % IN/ID = HN/HD * humn
    {IN, ID} = sub(I1, I2),
    {HN, HD} = sub(H2, H1),
    {N, D} = simplify({IN * HD, ID * HN}),
    erlang:display(N div D).
