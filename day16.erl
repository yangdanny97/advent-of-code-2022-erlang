-module(day16).
-compile(export_all).

input2() ->
    "Valve TM has flow rate=3; tunnels lead to valves GU, KQ, BV, MK\n"
    "Valve BX has flow rate=0; tunnels lead to valves CD, HX\n"
    "Valve GV has flow rate=8; tunnels lead to valves MP, SE\n"
    "Valve OI has flow rate=0; tunnels lead to valves ZB, RG\n"
    "Valve OY has flow rate=0; tunnels lead to valves XG, ZB\n"
    "Valve EZ has flow rate=0; tunnels lead to valves OU, LI\n"
    "Valve TN has flow rate=0; tunnels lead to valves DT, GU\n"
    "Valve SE has flow rate=0; tunnels lead to valves GV, CD\n"
    "Valve SG has flow rate=0; tunnels lead to valves XR, NK\n"
    "Valve EB has flow rate=0; tunnels lead to valves SJ, CE\n"
    "Valve QB has flow rate=0; tunnels lead to valves AW, MI\n"
    "Valve GU has flow rate=0; tunnels lead to valves TN, TM\n"
    "Valve AW has flow rate=11; tunnels lead to valves QB, IG, IK, VK\n"
    "Valve IG has flow rate=0; tunnels lead to valves AW, SH\n"
    "Valve MJ has flow rate=0; tunnels lead to valves IK, XR\n"
    "Valve HX has flow rate=0; tunnels lead to valves BX, AA\n"
    "Valve IK has flow rate=0; tunnels lead to valves MJ, AW\n"
    "Valve QZ has flow rate=0; tunnels lead to valves AF, XG\n"
    "Valve CV has flow rate=0; tunnels lead to valves KT, AA\n"
    "Valve ES has flow rate=0; tunnels lead to valves BV, CD\n"
    "Valve NK has flow rate=0; tunnels lead to valves YQ, SG\n"
    "Valve SL has flow rate=0; tunnels lead to valves DT, XL\n"
    "Valve RG has flow rate=17; tunnels lead to valves SJ, OI, WC\n"
    "Valve ZB has flow rate=9; tunnels lead to valves OY, MP, DI, OX, OI\n"
    "Valve SJ has flow rate=0; tunnels lead to valves RG, EB\n"
    "Valve GF has flow rate=19; tunnels lead to valves DQ, SH, IH\n"
    "Valve OU has flow rate=10; tunnels lead to valves EZ, TL, WC\n"
    "Valve TL has flow rate=0; tunnels lead to valves OU, OX\n"
    "Valve XG has flow rate=18; tunnels lead to valves QZ, OY\n"
    "Valve EK has flow rate=20; tunnels lead to valves FD, MI\n"
    "Valve BV has flow rate=0; tunnels lead to valves TM, ES\n"
    "Valve AA has flow rate=0; tunnels lead to valves CV, HX, TR, MK, DQ\n"
    "Valve UO has flow rate=23; tunnel leads to valve AF\n"
    "Valve LI has flow rate=0; tunnels lead to valves EZ, CE\n"
    "Valve MI has flow rate=0; tunnels lead to valves EK, QB\n"
    "Valve MP has flow rate=0; tunnels lead to valves GV, ZB\n"
    "Valve YQ has flow rate=14; tunnels lead to valves VK, MG, NK\n"
    "Valve AF has flow rate=0; tunnels lead to valves UO, QZ\n"
    "Valve SH has flow rate=0; tunnels lead to valves IG, GF\n"
    "Valve FD has flow rate=0; tunnels lead to valves IH, EK\n"
    "Valve KQ has flow rate=0; tunnels lead to valves TM, FQ\n"
    "Valve DI has flow rate=0; tunnels lead to valves ZB, CD\n"
    "Valve KT has flow rate=0; tunnels lead to valves DT, CV\n"
    "Valve MG has flow rate=0; tunnels lead to valves NQ, YQ\n"
    "Valve DQ has flow rate=0; tunnels lead to valves GF, AA\n"
    "Valve CE has flow rate=21; tunnels lead to valves LI, EB\n"
    "Valve MK has flow rate=0; tunnels lead to valves AA, TM\n"
    "Valve XL has flow rate=0; tunnels lead to valves CD, SL\n"
    "Valve OX has flow rate=0; tunnels lead to valves TL, ZB\n"
    "Valve DT has flow rate=5; tunnels lead to valves NQ, TP, KT, SL, TN\n"
    "Valve IH has flow rate=0; tunnels lead to valves GF, FD\n"
    "Valve TP has flow rate=0; tunnels lead to valves XR, DT\n"
    "Valve FQ has flow rate=0; tunnels lead to valves XR, KQ\n"
    "Valve CD has flow rate=6; tunnels lead to valves DI, BX, XL, ES, SE\n"
    "Valve XR has flow rate=7; tunnels lead to valves TR, FQ, TP, MJ, SG\n"
    "Valve VK has flow rate=0; tunnels lead to valves YQ, AW\n"
    "Valve WC has flow rate=0; tunnels lead to valves RG, OU\n"
    "Valve TR has flow rate=0; tunnels lead to valves XR, AA\n"
    "Valve NQ has flow rate=0; tunnels lead to valves DT, MG".

input() ->
    "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB\n"
    "Valve BB has flow rate=13; tunnels lead to valves CC, AA\n"
    "Valve CC has flow rate=2; tunnels lead to valves DD, BB\n"
    "Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE\n"
    "Valve EE has flow rate=3; tunnels lead to valves FF, DD\n"
    "Valve FF has flow rate=0; tunnels lead to valves EE, GG\n"
    "Valve GG has flow rate=0; tunnels lead to valves FF, HH\n"
    "Valve HH has flow rate=22; tunnel leads to valve GG\n"
    "Valve II has flow rate=0; tunnels lead to valves AA, JJ\n"
    "Valve JJ has flow rate=21; tunnel leads to valve II".

processLine(Line, Map) ->
    S = re:replace(Line, "Valve ", "", [global, {return, list}]),
    S2 = re:replace(S, " has flow rate=", ", ", [global, {return, list}]),
    S3 = re:replace(S2, "; tunnels lead to valves ", ", ", [global, {return, list}]),
    S4 = re:replace(S3, "; tunnel leads to valve ", ", ", [global, {return, list}]),
    [Valve, Flow | Tunnels] = string:tokens(S4, ", "),
    maps:put(Valve, {list_to_integer(Flow), ordsets:from_list(Tunnels)}, Map).

getFlow(Valve, Map) ->
    {Flow, _} = maps:get(Valve, Map),
    Flow.

getTunnels(Valve, Map) ->
    {_, Tunnels} = maps:get(Valve, Map),
    Tunnels.

search(_, 0, TotalFlow, _, _) ->
    TotalFlow;
search(_, _, TotalFlow, [], _) ->
    TotalFlow;
search([Current | Previous] = Path, TimeLeft, TotalFlow, Unopened, Map) ->
    {Flow, Tunnels} = maps:get(Current, Map),
    CanOpen =
        case Flow /= 0 of
            true -> [Current];
            false -> []
        end,
    Options = ordsets:union(ordsets:subtract(Tunnels, lists:sublist(Previous, 1)), CanOpen),
    lists:foldl(
        fun(Next, Best) ->
            {NextFlow, NextT} = maps:get(Next, Map),
            if
                Next == Current ->
                    max(
                        Best,
                        search(
                            [Next | Path],
                            TimeLeft - 1,
                            TotalFlow + (TimeLeft - 1) * Flow,
                            ordsets:del_element(Current, Unopened),
                            maps:put(Current, {0, Tunnels}, Map)
                        )
                    );
                (NextFlow == 0) andalso (length(NextT) == 1) ->
                    Best;
                true ->
                    max(Best, search([Next | Path], TimeLeft - 1, TotalFlow, Unopened, Map))
            end
        end,
        TotalFlow,
        Options
    ).

part1() ->
    S = input(),
    % valve -> {flow, is open, tunnels}
    Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
    Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
    erlang:display(search(["AA"], 30, 0, Unopened, Map)).

perms([]) -> [[]];
perms(L) -> [[H | T] || H <- L, T <- perms(L -- [H])].

testperms() ->
    S = input(),
    % valve -> {flow, is open, tunnels}
    Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
    Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
    erlang:display(length(perms(Unopened))).

search2(_, _, 0, TotalFlow, _, _) ->
    TotalFlow;
search2(_, _, _, TotalFlow, 0, _) ->
    TotalFlow;
search2(
    [Current | Previous] = Path, [ECurrent | EPrevious] = EPath, TimeLeft, TotalFlow, Unopened, Map
) ->
    {Flow, Tunnels} = maps:get(Current, Map),
    {EFlow, ETunnels} = maps:get(Current, Map),
    CanOpen =
        case Flow /= 0 of
            true -> [Current];
            false -> []
        end,
    ECanOpen =
        case (EFlow /= 0) and (ECurrent /= Current) of
            true -> [ECurrent];
            false -> []
        end,
    Options = CanOpen ++ (Tunnels -- lists:sublist(Previous, 1)),
    EOptions = ECanOpen ++ (ETunnels -- lists:sublist(EPrevious, 1)),
    AllOptions = ordsets:from_list(
        lists:flatten(lists:map(fun(O) -> lists:map(fun(E) -> {O, E} end, EOptions) end, Options))
    ),
    Filter = ordsets:from_list(
        lists:map(fun({A, B}) -> {B, A} end, lists:filter(fun({A, B}) -> A < B end, AllOptions))
    ),
    FilteredOptions = ordsets:subtract(AllOptions, Filter),
    lists:foldl(
        fun({Next, ENext}, Best) ->
            if
                (Next == Current) and (ENext == ECurrent) ->
                    max(
                        Best,
                        search2(
                            [Next | Path],
                            [ENext | EPath],
                            TimeLeft - 1,
                            TotalFlow + (TimeLeft - 1) * Flow + (TimeLeft - 1) * EFlow,
                            ordsets:del_element(Current, ordsets:del_element(ECurrent, Unopened)),
                            maps:put(Current, {0, Tunnels}, maps:put(ECurrent, {0, ETunnels}, Map))
                        )
                    );
                (Next == Current) ->
                    max(
                        Best,
                        search2(
                            [Next | Path],
                            [ENext | EPath],
                            TimeLeft - 1,
                            TotalFlow + (TimeLeft - 1) * Flow,
                            ordsets:del_element(Current, Unopened),
                            maps:put(Current, {0, Tunnels}, Map)
                        )
                    );
                (ENext == ECurrent) ->
                    max(
                        Best,
                        search2(
                            [Next | Path],
                            [ENext | EPath],
                            TimeLeft - 1,
                            TotalFlow + (TimeLeft - 1) * EFlow,
                            ordsets:del_element(ECurrent, Unopened),
                            maps:put(ECurrent, {0, ETunnels}, Map)
                        )
                    );
                % (NextFlow == 0) andalso (length(NextT) == 1) -> Best;
                true ->
                    max(
                        Best,
                        search2(
                            [Next | Path], [ENext | EPath], TimeLeft - 1, TotalFlow, Unopened, Map
                        )
                    )
            end
        end,
        TotalFlow,
        FilteredOptions
    ).

part2() ->
    S = input(),
    % valve -> {flow, is open, tunnels}
    Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
    Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
    erlang:display(search2(["AA"], ["AA"], 15, 0, Unopened, Map)).
