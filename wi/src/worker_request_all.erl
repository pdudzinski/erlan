-module(worker_request_all).

-export([start_link/0, request_all/1, available/3, available/2,
         busy/3]).
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    {ok, available, unknown}.

request_all(Id) ->
    gen_fsm:sync_send_event(?SERVER, {request_all, Id}).

available({request_all, Id}, _From, _State) ->
    %gen_fsm:send_event(?SERVER, {busy}),
    %Account = wi_server:get_account_by_id(Id),
    io:format("Requestin all: ~p~n", [Id]),
    {reply, {ok, requested}, available, Id};
available(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, available, State}.

available({busy}, State) ->
    {next_state, busy, State};
available(_Event, State) ->
    {next_state, busy, State}.

busy({request_all, _}, _From, State) ->
    io:format("Request all is running for ~p...~n", [State]),
    {reply, {warning, request_all_running}, busy, State};
busy(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, busy, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    {reply, ok, StateName, State}.

handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
