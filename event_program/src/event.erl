-module(event).
-compile(export_all).

-record(state, 
       {server, name="", to_go=[0]}).

start(EventName, DateTime) -> spawn(?MODULE, init, [self(), EventName, DateTime]).

start_link(EventName, DateTime) -> spawn_link(?MODULE, init, [self(), EventName, DateTime]).
       
init(Server, EventName, DateTime) ->
    loop(#state{server=Server, name=EventName, to_go=time_to_go(DateTime)}).

%% Because Erlang is limited to about 49 days (49*24*60*60*1000) in
%% milliseconds, the following function is used
normalize(N) ->
    Limit = 49*24*60*60,
    [N rem Limit | lists:duplicate(N div Limit, Limit)].
    
time_to_go(TimeOut={{_,_,_}, {_,_,_}}) ->
    Now = calendar:local_time(),
    ToGo = calendar:datetime_to_gregorian_seconds(TimeOut) -
    calendar:datetime_to_gregorian_seconds(Now),
    Secs = if ToGo > 0  -> ToGo;
              ToGo =< 0 -> 0
           end,
    normalize(Secs).

loop(State = #state{server=Server, to_go=[Time|Next]}) ->
    receive
        {Server, Ref, cancel} ->
            io:format('Canceled!'),
            Server ! {Ref, ok}
    after Time * 1000 -> 
        if Next =:= [] ->
            io:format('Alarm!! ~p~n', [State#state.name]),
            Server ! {done, State#state.name};
           Next =/= [] ->
            loop(State#state{server=Server, to_go=Next})
        end
    end.
    
cancel(Pid) -> 
    %% Monitor process if it's not dead already
    Ref = erlang:monitor(process, Pid),
    Pid ! {self(), Ref, cancel},
    receive
        {Ref, ok} -> 
            erlang:demonitor(Ref, [flush]),
            ok;
        {'DOWN', Ref, process, Pid, _Reason} ->
            ok
    end.
    
%% Pid = event:start('Obodz mnie za 10 sekund', 500).
%% event:cancel(Pid).
