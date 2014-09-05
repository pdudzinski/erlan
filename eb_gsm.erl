-module(eb_gsm).

%% API
-export([start_link/0, authorize/2, deposit/1, withdraw/1,
         cancel/0, balance/0]).

%% gen_fsm callbacks
-export([init/1, unauthorized/2, unauthorized/3, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         authorized/2, authorized/3, done/2]).
         
-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) -> 
    {ok, unauthorized, nobody}.

%sync_send_event method is equivalent to the call method of gen_server.
%It sends the message, the second argument, to the current state of 
%the server represented with the first argument
authorize(Name, PIN) -> 
    gen_fsm:sync_send_event(?SERVER, {authorize, Name, PIN}).
    
%deposit  is sent to current state, async    
deposit(Amount) ->
  gen_fsm:send_event(?SERVER, {deposit, Amount}).

withdraw(Amount) ->
  gen_fsm:send_event(?SERVER, {withdraw, Amount}).
  
%send_all_state_event sends a message whatever state you are in
%handled by handle_event
cancel() ->
  gen_fsm:send_all_state_event(?SERVER, cancel).
  
balance() ->
    gen_fsm:sync_send_event(?SERVER, balance).
  
%%%%%%%%%%%%%%%
  
unauthorized({authorize, Name, Pin}, _From, State) ->
    case eb_server:authorize(Name, Pin) of
    ok ->
      {reply, ok, authorized, Name};
    {error, Reason} ->
      {reply, {error, Reason}, unauthorized, State}
    end;
unauthorized(balance, _From, State) ->
    Reply = {error, check_balance_only_when_authorized},
    {reply, Reply, unauthorized, State};
unauthorized(_Event, _From, State) ->
    Reply = {error, invalid_message},
    %{reply, Response, NewStateName, NewStateData}
    {reply, Reply, unauthorized, State}.
    
unauthorized({deposit, Amount}, State) ->
    io:format('Account not authorized ~p~n',[State]),
    {next_state, unauthorized, State};
unauthorized({withdraw, Amount}, State) ->
    io:format('Account not authorized ~p~n',[State]),
    {next_state, unauthorized, State};
unauthorized(_Event, State) ->
    {next_state, state_name, State}.
    

authorized(balance, _From, State) ->
    {Msg, {Amount, PIN}} = eb_server:check_account_balance(State),
    {reply, Amount, authorized, State};
authorized(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, unauthorized, State}.
    
authorized({deposit, Amount}, State) ->
    eb_server:deposit_money(State, Amount),
    %If no message is received in 5000 milliseconds (or 5 seconds),
    %then a timeout message is sent to the current state
    {next_state, done, State, 5000};    
authorized({withdraw, Amount}, State) ->
    eb_server:withdraw_money(State, Amount),
    {next_state, done, State, 5000};
authorized(_Event, State) ->
    {next_state, unauthorized, nobody}.
    
done(timeout, _State) ->
    io:format('Done - timeout'),
    {next_state, unauthorized, nobody};
done(_Event, _State) ->
    io:format('Done - dev/null'),
    {next_state, unauthorized, nobody}.
    
handle_sync_event(Event, From, StateName, State) ->
    io:format('Handling sync event ~n'),
    Reply = ok,
    {reply, Reply, StateName, State}.
  
handle_event(cancel, _StateName, _State) ->
  {next_state, unauthorized, nobody};
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.
    
terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


c(eb_server).
Server = eb_server:start_link().
eb_server:create_account('Pawel', '1234').
eb_server:deposit_money('Pawel', 100).
eb_server:withdraw_money('Pawel', 10).
eb_server:authorize('Pawel', '1234').

c(eb_gsm).
Atm = eb_gsm:start_link().
eb_gsm:authorize('Adam', 1234).
eb_gsm:authorize('Pawel', '1234').
eb_gsm:deposit(20).
eb_gsm:balance().
