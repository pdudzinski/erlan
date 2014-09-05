-module(atm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, insert_digit/2, unauthorized/3,
         authorize/1, make_pin_of_list/2, get_pin/1]).
  
%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-define(PIN_LENGTH, 4).

start_link() ->
  gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).
  
init([]) -> 
    State = dict:new(),
    {ok, unauthorized, State}.
    
%% Calls

insert_digit(Name, Digit) ->
    gen_fsm:sync_send_event(?MODULE, {insert_digit, Name, Digit}).
    
authorize(Name) ->
    gen_fsm:sync_send_event(?MODULE, {authorize, Name}).
    
%% Helpers
get_pin(L) -> make_pin_of_list(L, 0).
make_pin_of_list([], Acc) -> Acc;
make_pin_of_list([H|T], Acc) ->
    Length = length([H|T]),
    case Length of
        4 -> make_pin_of_list(T, H * 1000 + Acc);
        3 -> make_pin_of_list(T, H * 100 + Acc);
        2 -> make_pin_of_list(T, H * 10 + Acc);
        1 -> make_pin_of_list(T, H * 1 + Acc);
        _ -> make_pin_of_list(T, H * 1 + Acc)
    end.
    
%% States

unauthorized({insert_digit, Name, Digit}, _From, State) ->
    case eb_server:get_user_and_pin(Name) of
        {UserName, PIN} -> 
            io:format('User: ~p PIN: ~p digit: ~p ~n', [UserName, PIN, Digit]),
            Entry = dict:find(Name, State),
            case Entry of
                {ok, Digits} when length(Digits) < 4 ->
                    NewDigits = Digits ++ [Digit],
                    NewState = dict:store(Name, NewDigits, State),
                    {reply, ok, unauthorized, NewState};
                {ok, Digits} when length(Digits) =:= 4 ->
                    io:format('You cannot push more digits, please authorize~n'),
                    {reply, ok, unauthorized, State};
                error ->
                    NewState = dict:store(Name, [Digit], State),
                    {reply, ok, unauthorized, NewState}
            end;
        {account_does_not_exist} ->
            io:format('Account not found'),
            {reply, ok, unauthorized, State}
    end;
unauthorized({authorize, Name}, _From, State) ->
    case eb_server:get_user_and_pin(Name) of
        {UserName, PIN} -> 
            Entry = dict:find(Name, State),
            case Entry of
                {ok, Digits} when length(Digits) =:= 4 ->
                    InsertedPIN = get_pin(Digits),
                    io:format('Will try to authorize.. ~p with pin ~p and check with ~p~n', [UserName, PIN, InsertedPIN]),
                    Authorization = eb_server:authorize(Name, InsertedPIN),
                    case Authorization of 
                        ok -> 
                            io:format('Huraaaaaaaaa!'),
                            {reply, ok, authorized, State};
                        {error,invalid_pin} ->
                            io:format('Sorry, invalid PIN~n'),
                            {reply, invalid_pin, unauthorized, dict:new()}
                    end;
                _ ->
                    io:format('Cannot authorize, pin is to short~n'),
                    {reply, pin_to_short, unauthorized, State}
            end;
        {account_does_not_exist} ->
            io:format('Account not found'),
            {reply, ok, unauthorized, State}
    end;

unauthorized(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, unauthorized, State}.

%% Handlers

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.
  
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
    
terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
    
