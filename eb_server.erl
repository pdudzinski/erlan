-module(eb_server).

-export([start_link/0, init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_call/3]).
-export([create_account/2, deposit_money/2, withdraw_money/2,
         check_account_balance/1, authorize/2, get_user_and_pin/1]).
         
-behaviour(gen_server).

-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_Args) ->
    %State is always passed to handlers
    State = dict:new(),
    {ok, State}.
    
create_account(Name, PIN) -> 
    gen_server:cast(?SERVER, {create_account, Name, PIN}).

deposit_money(Name, Amount) ->
    gen_server:call(?SERVER, {deposit, Name, Amount}).
    
withdraw_money(Name, Amount) ->
    gen_server:call(?SERVER, {withdraw, Name, Amount}).
    
check_account_balance(Name) ->
    gen_server:call(?SERVER, {check, Name}).

authorize(Name, PIN) ->
    gen_server:call(?SERVER, {authorize, Name, PIN}).

get_user_and_pin(Name) ->
    gen_server:call(?SERVER, {get_user, Name}).

handle_cast({create_account, Name, PIN}, State) -> 
    NewBalance = 0,
    %dict:store(key, value, collection)
    {noreply, dict:store(Name, {NewBalance, PIN}, State)};
handle_cast(_, State) ->
    {noreply, State}.
    
handle_call({deposit, Name, Amount}, _From, State) ->
  FoundAccount = dict:find(Name, State),
  case FoundAccount of
    {ok, {Value, PIN}} ->
      Balance = Value + Amount,
      NewState = dict:store(Name, {Balance, PIN}, State),
      %{reply, response, new_state}
      {reply, {ok, Balance, PIN}, NewState};
    error ->
      {reply, {error, account_does_not_exist}, State}
  end;
  
handle_call({withdraw, Name, Amount}, _From, State) ->
    FoundAccount = dict:find(Name, State),
    case FoundAccount of
        {ok, {Value, PIN}} when Value - Amount >= 0 ->
            Balance = Value - Amount,
            NewState = dict:store(Name, {Balance, PIN}, State),
            {reply, {ok, {Balance, PIN}}, NewState};
        {ok, {Value, _}} when Value - Amount < 0 ->
          {reply, {error, not_enough_money}, State};
        error ->
          {reply, {error, account_does_not_exist}, State}
    end;
handle_call({check, Name}, _From, State) ->
    FoundAccount = dict:find(Name, State),
    case FoundAccount of
        {ok, _} -> 
          {reply, FoundAccount, State};
        error ->
          {reply, {error, account_does_not_exist}, State}
    end;
handle_call({authorize, Name, PIN}, _From, State) ->
    FoundAccount = dict:find(Name, State),
    case FoundAccount of
        {ok, {_, PIN}} -> 
          {reply, ok, State};
        {ok, {_, _}} -> 
          {reply, {error, invalid_pin}, State};
        error ->
          {reply, {error, account_does_not_exist}, State}
    end;
handle_call({get_user, Name}, _From, State) -> 
    FoundAccount = dict:find(Name, State),
    case FoundAccount of 
        {ok, {_, PIN}} ->
            {reply, {Name, PIN}, State};
        error ->
            {reply, {account_does_not_exist}, State}
    end;
handle_call(_Request, _From, State) ->
  Reply = {ok, State},
  {reply, Reply, State}.
    
handle_info(_Info, State) ->
    {noreply, State}.
  
terminate(_Reason, _State) ->
    io:format('Server terminated! ~n~n~n~n~n'),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%c(eb_server).
%Server = eb_server:start_link().
%eb_server:create_account('Pawel', '1234').
%eb_server:deposit_money('Pawel', 100).
%eb_server:withdraw_money('Pawel', 10).
