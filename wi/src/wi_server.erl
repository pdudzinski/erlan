-module(wi_server).

-export([start_link/0, init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_call/3]).
-export([create_account/2, get_account_by_id/1]).

-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {
          accounts = dict:new()  :: dict()
         }).

%%%===================================================================
%%% API gen_server
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% API
%%%===================================================================
create_account(Id, Name) ->
    gen_server:call(?SERVER, {create_account, Id, Name}).

-spec get_account_by_id(integer()) -> {ok, pid()} | {error, not_found}.
get_account_by_id(Id) ->
    gen_server:call(?SERVER, {get_account_by_id, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
init(_Args) ->
    {ok, #state{}}.

handle_cast(_, State) ->
    {noreply, State}.

handle_call({create_account, Id, Name}, _From, #state{accounts=Accounts1}=State1) ->
    case dict:find(Id, Accounts1) of
        {ok, {_Pid, _Name}} ->
            {reply, {error, already_exists}, State1};
        error ->
            {ok, Pid} = wi_accounts_sup:start_child(Id, Name),
            Accounts2=dict:store(Id, {Pid, Name}, Accounts1),
            {reply, {ok, Pid}, State1#state{accounts=Accounts2}}
    end;
handle_call({get_account_by_id, Id}, _From, #state{accounts=Accounts}=State) ->
    Reply = case dict:find(Id, Accounts) of
                {ok, {Pid, _Name}} ->
                    {ok, Pid};
                error ->
                    {error, not_found}
            end,
    {reply, Reply, State};
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

