-module(day25).
-compile(export_all).

input() ->
    "1=-0-2\n"
    "12111\n"
    "2=0=\n"
    "21\n"
    "2=01\n"
    "111\n"
    "20012\n"
    "112\n"
    "1=-1=\n"
    "1-12\n"
    "12\n"
    "1=\n"
    "122".

pow(_, 0) -> 1;
pow(M, N) -> M * pow(M, N - 1).

string_to_snafu([]) ->
    0;
string_to_snafu([H | T] = S) ->
    Digit =
        case H of
            $2 -> 2;
            $1 -> 1;
            $0 -> 0;
            $- -> -1;
            $= -> -2
        end,
    Digit * pow(5, length(S) - 1) + string_to_snafu(T).

% get list of base 5 digits in reverse order
b5helper(0) -> [];
b5helper(N) -> [N rem 5 | b5helper(N div 5)].

integer_to_snafu(N) ->
    Digits = b5helper(N),
    {Digits2, Carry} = lists:foldl(
        fun(Dig, {Acc, Carry}) ->
            Val = Dig + Carry,
            {Dig2, C1} =
                case Val rem 5 of
                    3 -> {-2, 1};
                    4 -> {-1, 1};
                    _ -> {Val rem 5, 0}
                end,
            {[Dig2 | Acc], C1 + (Val div 5)}
        end,
        {[], 0},
        Digits
    ),
    % handle possible extra carry digit at the end
    Digits3 = case Carry of
        0 -> Digits2;
        _ -> [Carry | Digits2]
    end,
    [
        case D of
            0 -> $0;
            1 -> $1;
            2 -> $2;
            -1 -> $-;
            -2 -> $=
        end
     || D <- Digits3
    ].

part1() ->
    Numbers = [string_to_snafu(L) || L <- string:tokens(input(), "\n")],
    Sum = lists:sum(Numbers),
    erlang:display(integer_to_snafu(Sum)).
