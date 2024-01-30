% navigation when there are NO obstacles
-module(navigation_without_obstacles).
-export([navigate/2]).

navigate(Source, Destination) ->
    io:format("navigating from ~p to ~p.~n", [Source, Destination]),
    New_pos = move_vert(Source, Destination),
    New_pos2 = move_hor(New_pos, Destination),
    verify(New_pos2, Destination).

move_vert(Source, Destination) ->
    {X1, Y1} = Source,
    {_, Y2} = Destination,
    if
        Y2 > Y1 ->
            io:format("UP "),
            move_vert({X1, Y1+1}, Destination);
        Y2 < Y1 ->
            io:format("DOWN "),
            move_vert({X1, Y1-1}, Destination);
        true ->
            {X1, Y1}
end.

move_hor(Source, Destination) ->
    {X1, Y1} = Source,
    {X2, _} = Destination,
    if
        X2 > X1 ->
            io:format("RIGHT "),
            move_hor({X1+1, Y1}, Destination);
        X2 < X1 ->
            io:format("LEFT "),
            move_hor({X1-1, Y1}, Destination);
        true ->
            {X1, Y1}
end.
            
verify(Source, Destination) ->
    if
        Source =:= Destination -> 
            io:format("~nDestination ~p has been reached.~n", [Destination]);
        true ->
            io:format("~nError ! Could not reach Destination.~n")
    end,
    io:format("End of program !~n").
