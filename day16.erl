-module(day16).
-compile(export_all).

input() ->
    "Valve TM has flow rate=3; tunnels lead to valves GU, KQ, BV, MK
Valve BX has flow rate=0; tunnels lead to valves CD, HX
Valve GV has flow rate=8; tunnels lead to valves MP, SE
Valve OI has flow rate=0; tunnels lead to valves ZB, RG
Valve OY has flow rate=0; tunnels lead to valves XG, ZB
Valve EZ has flow rate=0; tunnels lead to valves OU, LI
Valve TN has flow rate=0; tunnels lead to valves DT, GU
Valve SE has flow rate=0; tunnels lead to valves GV, CD
Valve SG has flow rate=0; tunnels lead to valves XR, NK
Valve EB has flow rate=0; tunnels lead to valves SJ, CE
Valve QB has flow rate=0; tunnels lead to valves AW, MI
Valve GU has flow rate=0; tunnels lead to valves TN, TM
Valve AW has flow rate=11; tunnels lead to valves QB, IG, IK, VK
Valve IG has flow rate=0; tunnels lead to valves AW, SH
Valve MJ has flow rate=0; tunnels lead to valves IK, XR
Valve HX has flow rate=0; tunnels lead to valves BX, AA
Valve IK has flow rate=0; tunnels lead to valves MJ, AW
Valve QZ has flow rate=0; tunnels lead to valves AF, XG
Valve CV has flow rate=0; tunnels lead to valves KT, AA
Valve ES has flow rate=0; tunnels lead to valves BV, CD
Valve NK has flow rate=0; tunnels lead to valves YQ, SG
Valve SL has flow rate=0; tunnels lead to valves DT, XL
Valve RG has flow rate=17; tunnels lead to valves SJ, OI, WC
Valve ZB has flow rate=9; tunnels lead to valves OY, MP, DI, OX, OI
Valve SJ has flow rate=0; tunnels lead to valves RG, EB
Valve GF has flow rate=19; tunnels lead to valves DQ, SH, IH
Valve OU has flow rate=10; tunnels lead to valves EZ, TL, WC
Valve TL has flow rate=0; tunnels lead to valves OU, OX
Valve XG has flow rate=18; tunnels lead to valves QZ, OY
Valve EK has flow rate=20; tunnels lead to valves FD, MI
Valve BV has flow rate=0; tunnels lead to valves TM, ES
Valve AA has flow rate=0; tunnels lead to valves CV, HX, TR, MK, DQ
Valve UO has flow rate=23; tunnel leads to valve AF
Valve LI has flow rate=0; tunnels lead to valves EZ, CE
Valve MI has flow rate=0; tunnels lead to valves EK, QB
Valve MP has flow rate=0; tunnels lead to valves GV, ZB
Valve YQ has flow rate=14; tunnels lead to valves VK, MG, NK
Valve AF has flow rate=0; tunnels lead to valves UO, QZ
Valve SH has flow rate=0; tunnels lead to valves IG, GF
Valve FD has flow rate=0; tunnels lead to valves IH, EK
Valve KQ has flow rate=0; tunnels lead to valves TM, FQ
Valve DI has flow rate=0; tunnels lead to valves ZB, CD
Valve KT has flow rate=0; tunnels lead to valves DT, CV
Valve MG has flow rate=0; tunnels lead to valves NQ, YQ
Valve DQ has flow rate=0; tunnels lead to valves GF, AA
Valve CE has flow rate=21; tunnels lead to valves LI, EB
Valve MK has flow rate=0; tunnels lead to valves AA, TM
Valve XL has flow rate=0; tunnels lead to valves CD, SL
Valve OX has flow rate=0; tunnels lead to valves TL, ZB
Valve DT has flow rate=5; tunnels lead to valves NQ, TP, KT, SL, TN
Valve IH has flow rate=0; tunnels lead to valves GF, FD
Valve TP has flow rate=0; tunnels lead to valves XR, DT
Valve FQ has flow rate=0; tunnels lead to valves XR, KQ
Valve CD has flow rate=6; tunnels lead to valves DI, BX, XL, ES, SE
Valve XR has flow rate=7; tunnels lead to valves TR, FQ, TP, MJ, SG
Valve VK has flow rate=0; tunnels lead to valves YQ, AW
Valve WC has flow rate=0; tunnels lead to valves RG, OU
Valve TR has flow rate=0; tunnels lead to valves XR, AA
Valve NQ has flow rate=0; tunnels lead to valves DT, MG".

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
                            ordsets:remove_element(Current, Unopened),
                            maps:put(Current, {0, Tunnels}, Map)
                        )
                    );
                (NextFlow == 0) andalso (length(NextT) == 1) -> Best;
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
    erlang:display(search(["AA"], 1, 0, Unopened, Map)).

% perms([]) -> [[]];
% perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].

% testperms()->
%     S = input(),
%     % valve -> {flow, is open, tunnels}
%     Map = lists:foldl(fun processLine/2, maps:new(), string:tokens(S, "\n")),
%     Unopened = lists:filter(fun(X) -> getFlow(X, Map) /= 0 end, maps:keys(Map)),
%     erlang:display(length(perms(Unopened))).

% search2(_, _, 0, TotalFlow, _, _) ->
%     TotalFlow;
% search2(_, _, _, TotalFlow, 0, _) ->
%     TotalFlow;
% search2([Current | Previous] = Path, [ECurrent | EPrevious] = EPath, TimeLeft, TotalFlow, Unopened, Map) ->
%     {Flow, Tunnels} = maps:get(Current, Map),
%     CanOpen =
%         case Flow /= 0 of
%             true -> [Current];
%             false -> []
%         end,
%     {EFlow, ETunnels} = maps:get(ECurrent, Map),
%     ECanOpen =
%         case (EFlow /= 0) and (ECurrent /= Current) of
%             true -> [ECurrent];
%             false -> []
%         end,
%     Options = CanOpen ++ (Tunnels -- lists:sublist(Previous, 1)),
%     EOptions = ECanOpen ++ (ETunnels -- lists:sublist(EPrevious, 1)),

%     lists:foldl(
%         fun({Next, ENext}, Best) ->
%             {NextFlow, NextT} = maps:get(Next, Map),
%             if
%                 Next == Current ->
%                     max(
%                         Best,
%                         search2(
%                             [Next | Path],
%                             [ENext | EPath],
%                             TimeLeft - 1,
%                             TotalFlow + (TimeLeft - 1) * Flow,
%                             Unopened - 1,
%                             maps:put(Current, {0, Tunnels}, Map)
%                         )
%                     );
%                 (NextFlow == 0) andalso (length(NextT) == 1) -> Best;
%                 true ->
%                     max(Best, search2([Next | Path], [ENext | EPath], TimeLeft - 1, TotalFlow, Unopened, Map))
%             end
%         end,
%         TotalFlow,
%         Options
%     ).