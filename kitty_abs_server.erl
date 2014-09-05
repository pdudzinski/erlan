-module(kitty_abs_server).
-compile(export_all).

start_link(Module, InitialState) ->
    spawn_link(fun() -> init(Module, InitialState) end).

%Call method is synchronous by atom sync
call(Pid, Msg) ->
    Ref = erlang:monitor(process, Pid),
    Pid ! {sync, self(), Ref, Msg},
    receive
        {Ref, Reply} -> erlang:demonitor(Ref, [flush]), Reply;
        {'DOWN', Ref, process, Pid, Reason} -> erlang:error(Reason)
    after 5000 ->
        erlang:error(timeout)
    end.

%Cast method is asynchronous by atom async
cast(Pid, Msg) ->
    Pid ! {async, Msg},
    ok.

reply({Pid, Ref}, Reply) -> Pid ! {Ref, Reply}.

init(Module, InitialState) ->
    loop(Module, Module:init(InitialState)).

loop(Module, State) ->
    receive
        {async, Msg} -> loop(Module, Module:handle_cast(Msg, State));
        {sync, Pid, Ref, Msg} -> loop(Module, Module:handle_call(Msg, {Pid, Ref}, State))
    end.

The next thing to do is reimplement the kitty server
