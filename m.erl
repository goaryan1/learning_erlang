% making a 2-d grid using mnesia
% create a grid with extensive size, then place obstacles to limit the access for the robot

-module(m).
-export([init/0, insert_rows/1, print_grid/0, mark_obstacle/2, mark_size/2]).

-record(grid, {row_number = 0, c1 = false, c2 = false, c3 = false, c4 = false, c5 = false, c6 = false, c7 = false, c8 = false, c9 = false, c10 = false}).

init() ->
    mnesia:start(),
    mnesia:create_table(grid, [{attributes, record_info(fields, grid)}, {type, ordered_set}]).

insert_rows(N) ->
    Trans = fun() ->
        lists:foreach(fun(RowNumber) ->
        insert_row(RowNumber) end, lists:seq(1, N)) end,
    mnesia:transaction(Trans).

insert_row(RowNumber) ->
    Trans = fun() ->
    mnesia:write(#grid{row_number = RowNumber}) end,
    mnesia:transaction(Trans).

print_grid() ->
    Trans = fun() ->
    Rows = mnesia:all_keys(grid),
    lists:foreach(fun(RowNumber) ->
        print_row(RowNumber) end, Rows) end,
    mnesia:transaction(Trans).

print_row(RowNumber) ->
    Trans = fun() ->
    io:format("~p~n", mnesia:read(grid, RowNumber)) end,
    mnesia:transaction(Trans).

mark_obstacle(Row, Column) ->
    Trans = 
        fun() ->
            [P] = mnesia:read(grid, Row),
            case Column of
                1 -> mnesia:write(P#grid{c1 = true});
                2 -> mnesia:write(P#grid{c2 = true});
                3 -> mnesia:write(P#grid{c3 = true});
                4 -> mnesia:write(P#grid{c4 = true});
                5 -> mnesia:write(P#grid{c5 = true});
                6 -> mnesia:write(P#grid{c6 = true});
                7 -> mnesia:write(P#grid{c7 = true});
                8 -> mnesia:write(P#grid{c8 = true});
                9 -> mnesia:write(P#grid{c9 = true});
                10 -> mnesia:write(P#grid{c10 = true});
                _ -> io:format("Out of bound~n")
            end
        end,
    mnesia:transaction(Trans).

mark_size(Height, Width) ->
    lists:foreach(fun(RowNumber) ->
        mark_obstacle(RowNumber, Width+1) end, lists:seq(1, Height+1)),
    lists:foreach(fun(ColNumber) ->
        mark_obstacle(Height+1, ColNumber) end, lists:seq(1, Width+1)).
    
