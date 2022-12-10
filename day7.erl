-module(day7).
-compile(export_all).

input() ->
    "$ cd /\n"
    "$ ls\n"
    "dir a\n"
    "14848514 b.txt\n"
    "8504156 c.dat\n"
    "dir d\n"
    "$ cd a\n"
    "$ ls\n"
    "dir e\n"
    "29116 f\n"
    "2557 g\n"
    "62596 h.lst\n"
    "$ cd e\n"
    "$ ls\n"
    "584 i\n"
    "$ cd ..\n"
    "$ cd ..\n"
    "$ cd d\n"
    "$ ls\n"
    "4060174 j\n"
    "8033020 d.log\n"
    "5626152 d.ext\n"
    "7214296 k".

% this contains a bit of boilerplate to ensure that an entry always exists for each directory
processInstr(Instr, CurrPath, Dict) ->
    Path = lists:join("/", lists:reverse(CurrPath)),
    case Instr of
        ["$", "ls"] ->
            {CurrPath, Dict};
        ["$", "cd", ".."] ->
            {tl(CurrPath), Dict};
        ["$", "cd", "/"] ->
            {"/", Dict};
        ["$", "cd", D] ->
            DirPath = lists:join("/", lists:reverse([D | CurrPath])),
            DirContents = maps:get(DirPath, Dict, sets:new()),
            NewPathContents = sets:add_element(DirPath, maps:get(Path, Dict)),
            {[D | CurrPath], Dict#{Path => NewPathContents, DirPath => DirContents}};
        ["dir", D] ->
            DirPath = lists:join("/", lists:reverse([D | CurrPath])),
            DirContents = maps:get(DirPath, Dict, sets:new()),
            NewPathContents = sets:add_element(DirPath, maps:get(Path, Dict)),
            {CurrPath, Dict#{Path => NewPathContents, DirPath => DirContents}};
        [SizeStr, Name] ->
            Size = list_to_integer(SizeStr),
            NewPathContents = sets:add_element({Name, Size}, maps:get(Path, Dict)),
            {CurrPath, Dict#{Path => NewPathContents}}
    end.

getSize(Dict, Dir) ->
    {_, Contents} = maps:find(Dir, Dict),
    sets:fold(
        fun(X, Acc) ->
            case X of
                {_, Size} -> Acc + Size;
                DirName -> Acc + getSize(Dict, DirName)
            end
        end,
        0,
        Contents
    ).

part1() ->
    S = input(),
    Instructions = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, Dict} = lists:foldl(
        fun(Instr, {Path, Dict}) -> processInstr(Instr, Path, Dict) end,
        {[], #{"/" => sets:new()}},
        Instructions
    ),
    Sizes = maps:map(fun(K, _) -> getSize(Dict, K) end, Dict),
    FilteredSizes = lists:filter(fun(Size) -> Size =< 100000 end, maps:values(Sizes)),
    Sum = lists:foldl(fun(Size, Acc) -> Size + Acc end, 0, FilteredSizes),
    erlang:display(Sum).

part2() ->
    S = input(),
    Instructions = [string:tokens(X, " ") || X <- string:tokens(S, "\n")],
    {_, Dict} = lists:foldl(
        fun(Instr, {Path, Dict}) -> processInstr(Instr, Path, Dict) end,
        {[], #{"/" => sets:new()}},
        Instructions
    ),
    Sizes = maps:map(fun(K, _) -> getSize(Dict, K) end, Dict),
    NeedToFree = maps:get("/", Sizes) - 40000000,
    FilteredSizes = lists:filter(fun(Size) -> Size >= NeedToFree end, maps:values(Sizes)),
    erlang:display(hd(lists:sort(FilteredSizes))).
