-module(tic).
-export([first/0]).

first() ->
    receive
        {tic, X} ->
            io:format("tic: ~w~n", [X]),
            second()
end.

second() ->
    receive
        {toe, X} ->
            io:format("toe: ~w~n", [X]),
            last();
        {tac, X} ->
            io:format("tac: ~w~n", [X]),
            last()
        
end.

last() -> 
    receive
        X -> 
            io:format("end: ~w~n", [X])
        end.