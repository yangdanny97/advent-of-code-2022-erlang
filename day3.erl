-module(day3).
-compile(export_all).

addLetterScore(L, Acc) ->
    if
        L >= 97 -> L - 96 + Acc;
        true -> L - 38 + Acc
    end.

input() ->
    "vJrwpWtwJgWrhcsFMMfFFhFp\n"
    "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL\n"
    "PmmdzqPrVvPwwTWBwg\n"
    "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn\n"
    "ttgJtRGJQctTZtZT\n"
    "CrZsJsPPZsGzwwsLwLmpwMDw\n".

part1() ->
    S = input(),
    Split = string:tokens(S, "\n"),
    Sum = lists:foldl(
        fun(X, Sum) ->
            Len = length(X),
            {Fst, Snd} = {
                ordsets:from_list(lists:sublist(X, Len div 2)),
                ordsets:from_list(lists:sublist(X, Len div 2 + 1, Len div 2))
            },
            Intersection = ordsets:to_list(ordsets:intersection(Fst, Snd)),
            Sum + lists:foldl(fun addLetterScore/2, 0, Intersection)
        end,
        0,
        Split
    ),
    erlang:display(Sum).

part2Helper([A, B, C | Tl], Sum) ->
    S1 = ordsets:from_list(A),
    S2 = ordsets:from_list(B),
    S3 = ordsets:from_list(C),
    Intersection = ordsets:to_list(ordsets:intersection(ordsets:intersection(S1, S2), S3)),
    Score = lists:foldl(fun addLetterScore/2, 0, Intersection),
    part2Helper(Tl, Sum + Score);
part2Helper(_, Acc) ->
    Acc.

part2() ->
    S = input(),
    Split = string:tokens(S, "\n"),
    Sum = part2Helper(Split, 0),
    erlang:display(Sum).
