-module(proc).
-compile(export_all).

cancel(Pid) ->
    io:format('Trying to cancel~n'),
    Ref = make_ref(),
    Pid ! {self(), Ref, cancel},
    receive 
        {Ref, ok} -> io:format('All done~n')
    end.

server_looper() ->
    receive
        {start} -> io:format('Server starts~n'), server_looper();
        {Pid, cancel} -> cancel(Pid), server_looper()
    end.
    
event_looper() ->
    io:format('Event fired!~n'),
    receive
        {From, Ref, cancel} -> io:format('Event cancelled~n'), From ! {Ref, ok};
        {From, alive} -> io:format('Yes, I am alive'), From ! {ok}, event_looper();
        _ -> event_looper()
    end.

%Server = spawn(proc, server_looper, []).
%Event1 = spawn(proc, event_looper, []).
%Event2 = spawn(proc, event_looper, []).

receiver() ->
    receive
        {From, _} -> io:format(' receiver received [~p]', [From]), receiver()
    end.

looper() ->
    receive
        {From, message} -> io:format('looper received'), From ! {self(), ok}, looper();
        _ -> exit(looper_dead)
    end.
    
%LINKING PROCESSES
%link makes if Pid wil die, dier will die as well, not the other way
dier(Pid) ->
    link(Pid),
    receive
        {From, message} -> io:format('dier received'), From ! {self(), ok}, dier(Pid)
    end.

%MONITORING PROCESSES
%monitor makes if Pid wil die, monitorer will recevie message   
monitorer(Pid) ->
    monitor(process, Pid),
    receive
        {From, message} -> io:format('dier received'), From ! {self(), ok}, monitorer(Pid);
        _ -> io:format('the other one died')
    end.
 
%Just printing out the message, process ends when received
dolphin1() ->
    receive
        do_a_flip -> io:format("How about no?~n");
        fish -> io:format("So long and thanks for all the fish!~n");
        _ -> io:format("Heh, we're smarter than you humans.~n")
    end.

%Return notifications back, process ends when received
dolphin2() ->
    receive
        {From, do_a_flip} -> From ! "How about no?";
        {From, fish} -> From ! "So long and thanks for all the fish!";
        _ -> io:format("Heh, we're smarter than you humans.~n")
    end.

%Return notifications back, recursive do it again
dolphin3() ->
    receive
        {From, do_a_flip} -> From ! "How about no?", dolphin3();
        {From, fish} -> From ! "So long and thanks for all the fish!";
        _ -> io:format("Heh, we're smarter than you humans.~n"), dolphin3()
    end.


fridge2(FoodList) ->
    receive
        {From, {store, Food}} -> From ! {stored, Food},
                                 fridge2([Food|FoodList]);
        {From, {take, Food}} ->
            case lists:member(Food, FoodList) of
                true ->
                    From ! {taken, Food},
                    fridge2(lists:delete(Food, FoodList));
                false ->
                    From ! {not_found, Food},
                    fridge2(FoodList)
            end;
        terminate -> ok
    end.

start(FoodList) -> 
    spawn(?MODULE, fridge2, [FoodList]).

store(Food, Pid, From) -> 
    Pid ! {From, {store, Food}},
    receive
        {stored, Food} -> "yes, food stored!"
    end.
    
take(Food, Pid, From) ->
    Pid ! {From, {take, Food}},
    receive
        {stored, Food} -> "yes, food taken!"
    end.

