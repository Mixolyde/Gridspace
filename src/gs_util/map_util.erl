%%% Some map generating algorithms and related utility

-module(map_util).
-author("Brian E. Williams").
-compile([debug_info, export_all]).


print_maze(Maze) ->
    print_maze(Maze, [{open, $\s }, {wall, $*}, {outside, $.}]).
print_maze([], _PrintChars) ->
    io:format("Done~n");
print_maze(Maze, PrintChars) ->
    start_maze(Maze, PrintChars),
    print_middle_rows(Maze, PrintChars),
    finish_maze(Maze, PrintChars).
print_row([], _PrintChars) -> ok;
print_row([Cell | Rest], PrintChars) ->
    print_cell(Cell, PrintChars),
    print_row(Rest, PrintChars).

print_middle_rows([], _PrintChars) -> ok;
print_middle_rows([Row | Rest], PrintChars) ->
    {outside, PrintChar} = lists:keyfind(outside, 1, PrintChars),
    io:format("~c", [PrintChar]),
    print_row(Row, PrintChars),
    io:format("~c~n", [PrintChar]),
    print_middle_rows(Rest, PrintChars).

start_maze(Maze, PrintChars) ->
    Width = max_width(Maze),
    % print_ruler(Width),
    print_border(Width, PrintChars).
finish_maze(Maze, PrintChars) ->
    Width = max_width(Maze),
    print_border(Width, PrintChars).
    % print_ruler(Width).
print_border(Width, PrintChars) ->
    {outside, PrintChar} = lists:keyfind(outside, 1, PrintChars),
    io:format("~s~n", [lists:duplicate( Width + 2, PrintChar)]).

print_ruler(Width) when Width < 10 -> io:format("~n");
print_ruler(Width) ->
    io:format("0123456789"),
    print_ruler(Width - 10).

print_cell(Cell, PrintChars) ->
    {Cell, PrintChar} = lists:keyfind(Cell, 1, PrintChars),
    % io:format("test[~p]", [PrintChar]),
    io:format("~c", [PrintChar]).

open_maze(Width, Height) ->
    fill_maze(Width, Height, open).

wall_maze(Width, Height) ->
    fill_maze(Width, Height, wall).

fill_maze(_Width, 0, _Cell) ->
    [];
fill_maze(Width, Height, Cell) ->
    [lists:duplicate(Width, Cell) | fill_maze(Width, Height - 1, Cell)].

get_cell(_Maze, {X, _Y}, Outside) when X < 1 -> Outside;
get_cell(_Maze, {_X, Y}, Outside) when Y < 1 -> Outside;
get_cell(Maze, {_X, Y}, Outside) when Y > length(Maze) -> Outside;
get_cell(Maze, {X, Y}, Outside) ->
    Width = length(lists:nth(Y, Maze)),
    if
        X > Width -> Outside;
        true ->
            lists:nth(X, lists:nth(Y, Maze))
    end.

list_of_neighbors(Maze, {X, Y}, Outside) ->
    list_of_neighbors_in_range(Maze, {X, Y}, Outside, 1).

list_of_neighbors_in_range(Maze, {X, Y}, Outside, Range)
    when is_integer(Range), is_integer(X), is_integer(Y), is_list(Maze) ->
    RangeSeq = lists:seq(0 - Range, Range, 1),
    % io:format("Generated Range seq: ~p~n", [RangeSeq]),
    Perms = lists:append(lists:map(fun(MapX) -> lists:map(fun(MapY) -> {MapX, MapY} end, RangeSeq) end, RangeSeq)),
    % io:format("Generated Perms: ~p~n", [Perms]),
    lists:map(fun({PermX, PermY}) -> map_util:get_cell(Maze, {X + PermX, Y + PermY}, Outside) end, Perms).

set_cell(Maze, {X, Y}, Cell) ->
    {HeadRows, [ReplaceRow | TailRows]} = lists:split(Y - 1, Maze),
    {Head, [_ReplaceCell | Tail]} = lists:split(X - 1, ReplaceRow),
    HeadRows ++ [Head ++ [Cell] ++ Tail] ++ TailRows.

max_width([]) -> 0;
max_width([First | Rest]) ->
    max_width(Rest, length(First)).
max_width([], Max) ->
    Max;
max_width([First | Rest], Max)
    when Max > length(First) ->
        max_width(Rest, Max);
max_width([First | Rest], _Max) ->
    max_width(Rest, length(First)).