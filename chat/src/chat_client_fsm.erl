-module(chat_client_fsm).

-behaviour(gen_fsm).

%% API gen_fsm
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4,
         handle_info/3, terminate/3, code_change/4,
         active/2]).
         
%% API
-export([send_message/2]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API gen_fsm
%%====================================================================
start_link(User) ->
  gen_fsm:start_link(?MODULE, [User], []).
  
%%====================================================================
%% API (active, away, offline)
%%====================================================================

send_message(To, Msg) ->
    case pg2:get_members(To) of
        [Pid] ->
            gen_fsm:send_event(Pid, {send, To, Msg});
        {error, _} ->
            {error, unknown_addressee}
    end.
    
    
%%====================================================================
%% gen_fsm callbacks
%%====================================================================
init([User]) ->
  {ok, active, User}.

active({send, To, Msg}, State) ->
    io:format("~p:~n~p~n", [To, Msg]),
    {next_state, active, State};
active(_Event, State) ->
    {next_state, active, State}.
    
handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = ok,
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.
