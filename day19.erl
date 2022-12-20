-module(day19).
-compile(export_all).

input() ->
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.\n"
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.".

canBuild({O, C, B, _}, {CO, CC, CB}) -> (O - CO) >= 0 andalso (C - CC) >= 0 andalso B - CB >= 0.
build({O, C, B, G}, {CO, CC, CB}) -> {O - CO, C - CC, B - CB, G}.
add({O, C, B, G}, {NO, NC, NB, NG}) -> {O + NO, C + NC, B + NB, G + NG}.
sub({O, C, B, G}, {NO, NC, NB, NG}) -> {O - NO, C - NC, B - NB, G - NG}.

shouldBuild({0, 0, 0, 0}, _, _, _, _) ->
    true;
shouldBuild({0, 0, 0, 1}, _, _, _, _) ->
    true;
shouldBuild(BotDelta, Bots, Costs, Resources, Waited) ->
    Idx =
        case BotDelta of
            {1, 0, 0, 0} -> 1;
            {0, 1, 0, 0} -> 2;
            {0, 0, 1, 0} -> 3
        end,
    {_, BotCost} = lists:keyfind({0, 0, 0, 1}, 1, Costs),
    % if waited last minute, only build something that couldn't be built before
    CouldNotBuild = not (Waited andalso canBuild(sub(Resources, Bots), BotCost)),
    MaxCost = lists:max(lists:map(fun({_, C}) -> element(Idx, C) end, Costs)),
    element(Idx, Bots) < MaxCost andalso CouldNotBuild.

search(0, {_, _, _, Geodes}, _, _, _, _) ->
    Geodes;
search(M, {_, _, _, Geodes} = Resources, {_, _, _, GBots} = Bots, Costs, Waited, Best) ->
    Resources2 = add(Resources, Bots),
    {_, GeodeBot} = lists:keyfind({0, 0, 0, 1}, 1, Costs),
    UB = lists:sum(lists:seq(GBots, GBots + M - 1)),
    CanBuildGeode = canBuild(Resources, GeodeBot),
    if
        Geodes + UB < Best ->
            Best;
        CanBuildGeode ->
            Bots2 = add(Bots, {0, 0, 0, 1}),
            Resources3 = build(Resources2, GeodeBot),
            search(M - 1, Resources3, Bots2, Costs, false, Best);
        true ->
            lists:foldl(
                fun({BotDelta, Cost}, Max) ->
                    case
                        canBuild(Resources, Cost) andalso
                            shouldBuild(BotDelta, Bots, Costs, Resources, Waited)
                    of
                        true ->
                            Bots2 = add(Bots, BotDelta),
                            Resources3 = build(Resources2, Cost),
                            Waited2 = BotDelta == {0, 0, 0, 0},
                            max(Max, search(M - 1, Resources3, Bots2, Costs, Waited2, Max));
                        false ->
                            Max
                    end
                end,
                Best,
                Costs
            )
    end.

matIdx(M) ->
    case M of
        "ore" -> 1;
        "clay" -> 2;
        "obsidian" -> 3;
        "geode" -> 4
    end.

costs([], Acc) ->
    Acc;
costs([N, Type | Tl], Acc) ->
    Count = list_to_integer(N),
    costs(Tl, setelement(matIdx(Type), Acc, Count)).

parseRecipe(Recipe) ->
    [BotType | C] = string:tokens(Recipe, " "),
    BotDelta = setelement(matIdx(BotType), {0, 0, 0, 0}, 1),
    {BotDelta, costs(C, {0, 0, 0})}.

parseRecipes(Recipes) ->
    S = re:replace(Recipes, " Each ", "", [global, {return, list}]),
    S2 = re:replace(S, " robot costs ", " ", [global, {return, list}]),
    S3 = re:replace(S2, " and ", " ", [global, {return, list}]),
    lists:reverse([{{0, 0, 0, 0}, {0, 0, 0}} | [parseRecipe(R) || R <- string:tokens(S3, ".")]]).

parseBlueprint(Line) ->
    S = re:replace(Line, "Blueprint ", "", [global, {return, list}]),
    [Id, Recipes] = string:tokens(S, ":"),
    {list_to_integer(Id), parseRecipes(Recipes)}.

part1() ->
    Blueprints = [parseBlueprint(B) || B <- string:tokens(input(), "\n")],
    Quality = lists:map(
        fun({Id, Recipes}) ->
            Id * search(24, {0, 0, 0, 0}, {1, 0, 0, 0}, Recipes, false, 0)
        end,
        Blueprints
    ),
    erlang:display(lists:sum(Quality)).

part2() ->
    Blueprints = [parseBlueprint(B) || B <- string:tokens(input(), "\n")],
    First3 = lists:sublist(Blueprints, 3),
    Prod = lists:foldl(
        fun({_, Recipes}, Acc) ->
            Acc * search(32, {0, 0, 0, 0}, {1, 0, 0, 0}, Recipes, false, 0)
        end,
        1,
        First3
    ),
    erlang:display(Prod).
