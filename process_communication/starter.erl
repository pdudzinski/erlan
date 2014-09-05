-module(starter).
-export([do/0, start/0]).


start() ->
    spawn_link(?MODULE, do, []).

do() ->
    receive
        dolphin_started -> 
            io:format("And a confirmation~n");
        start_dolphin -> 
            io:format("Ok I will start dolphin!~n"),
            start_dolphin(self());
        _ -> io:format("Say Whaaa?")
    end,
    do().
    
    
start_dolphin(From) ->
    io:format("Dolphin Started by ~p~n", [From]),
    From ! dolphin_started.
