-module(chat_server).

-behaviour(gen_server).

%% API
-export([start_link/0, subscribe/1, unsubscribe/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {}).

-define(SERVER, ?MODULE).

%%====================================================================
%% API gen_server
%% erl -sname node1 -setcookie erly
%%====================================================================
start_link() ->
  gen_server:start_link({global, ?SERVER}, ?MODULE, [], []).

%%====================================================================
%% API
%%====================================================================

subscribe(User) ->
    gen_server:call({global, ?SERVER}, {subscribe, User}).
    
unsubscribe(User) ->
    gen_server:call({global, ?SERVER}, {unsubscribe, User}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([]) ->
  {ok, #state{}}.

handle_call({subscribe, User}, _From, State) ->
   case pg2:get_members(User) of
      [_] ->
        {reply, {ok, already_created}, State};
      {error, _} ->
        {ok, Pid} = chat_user_sup:start_child(User),
        pg2:create(User),
        pg2:join(User, Pid),
        {reply, {ok, created}, State}
    end;
handle_call({unsubscribe, User}, _From, State) ->
    case pg2:get_members(User) of
      [Pid] ->
        pg2:leave(User, Pid),
        pg2:delete(User),
        {reply, {ok, unsubscribed}, State};
      {error, _} ->
        {reply, {ok, already_unsubscribed}, State}
    end;
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
