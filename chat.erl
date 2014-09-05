-module(chat).
-compile(export_all).

send_message(To, Msg) -> 
    Ref = make_ref(),
    To ! {message, self(), Msg, Ref},
    receive
        {Ref, ok} -> io:format('Sent...~n~n');
        _ -> io:format('Say whaaat?!~n')
    after 1000 ->
        io:format('Done...')
    end.
    
start_user() ->
    spawn_link(?MODULE, loop, []).
    
loop() ->
    receive
        {message, From, Msg, Ref} ->
            io:format('Message from: ~p~n~p~n', [From, Msg]),
            From ! {Ref, ok},
            loop()
    end.
