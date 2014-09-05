-module(wi_account).

-export([request_all/2, start_trial/2]).

-export([start_link/2]).
-export([initial/3, trial/3]).

-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4]).

-behaviour(gen_fsm).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API gen_fsm
%%%===================================================================
start_link(Id, Name) ->
  gen_fsm:start_link(?MODULE, [{Id, Name}], []).

%%%===================================================================
%%% API
%%%===================================================================
request_all(Pid, Id) ->
    gen_fsm:sync_send_event(Pid, {request_all, Id}).

start_trial(Pid, Id) ->
    gen_fsm:sync_send_event(Pid, {start_trial, Id}).

%%%===================================================================
%%% callbacks
%%%===================================================================
init([{Id, Name}]) ->
    {ok, initial, {Id, Name}}.

initial({request_all, _}, _From, State) ->
    {reply, {error, request_all_not_possible}, initial, State};
initial({start_trial, Id}, _From, State) ->
    {reply, {ok, trial_started, Id}, trial, State};
initial(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, initial, State}.

trial({request_all, Id}, _From, State) ->
    worker_request_all:request_all(Id),
    {reply, {ok, started}, trial, State};
trial({start_trial, _}, _From, State) ->
    {reply, {error, trial_already_started}, trial, State};
trial(_Event, _From, State) ->
    Reply = {error, invalid_message},
    {reply, Reply, trial, State}.

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
