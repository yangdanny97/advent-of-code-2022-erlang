-module(day6).
-compile(export_all).

helper([_ | T] = List, N, Processed) ->
    case ordsets:size(ordsets:from_list(lists:sublist(List, N))) == N of
        true -> Processed + N;
        false -> helper(T, N, Processed + 1)
    end.

part1() ->
    S = "bvwbjplbgvbhsrlpgdmjqwftvncz",
    Result = helper(S, 4, 0),
    erlang:display(Result).

part2() ->
    S = "bvwbjplbgvbhsrlpgdmjqwftvncz",
    Result = helper(S, 14, 0),
    erlang:display(Result).
