-module(scoreboard_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/0, reset_or_turn_on/0, set_teams/2,
         stand_by_preparation/2, start_game/0, add_point/2,
         game_started/2]).

%% gen_fsm callbacks
-export([init/1, turned_off/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API
%%====================================================================


start_link() ->
  gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).
  
reset_or_turn_on() ->
  gen_fsm:send_all_state_event(?SERVER, stand_by_preparation).

start_game() ->
  gen_fsm:send_all_state_event(?SERVER, start_game).
  
set_teams(TeamA, TeamB) ->
  L = [{TeamA, 0}, {TeamB, 0}],
  gen_fsm:send_event(?SERVER, {set_teams, {TeamA, TeamB, dict:from_list(L)}}).
  
add_point(Team, N) ->
  gen_fsm:send_event(?SERVER, {add_point, {Team, N}}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([]) ->
  {ok, turned_off, not_set}.

turned_off(_Event, State) ->
  io:format('Scoreboard is turned off!~n'),
  {next_state, turned_off, State}.

stand_by_preparation({set_teams, Scoreboard}, State) ->
    io:format('State: ~p', [State]),
    case State of
        not_set -> 
            {TeamA, TeamB, _} = Scoreboard,
            io:format('Setting teams on scoreboard: ~p vs ~p~n', [TeamA, TeamB]),
            {next_state, stand_by_preparation, Scoreboard};
        _ ->
            io:format('Teams already set. Please reset scoreboard if you need a change~n'),
            {next_state, stand_by_preparation, State}       
    end;
stand_by_preparation(_Event, State) ->
    {next_state, state_name, State}.
game_started({add_point, {Team, N}}, State) -> 
    io:format('Adding points ~p~n', [State]),
    %NewState = dict:update(Team, fun(X) -> X + N end, State),
    {next_state, game_started, State};
game_started(_Event, State) ->
    io:format('Game is on~n!'),
    {next_state, game_started, State}.
    
handle_event(stand_by_preparation, _StateName, State) ->
  io:format('Standing by...~n'),
  {next_state, stand_by_preparation, State};
handle_event(start_game, _StateName, State) ->
  {next_state, game_started, State};
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


c(scoreboard_fsm).
scoreboard_fsm:start_link().
scoreboard_fsm:reset_or_turn_on().
scoreboard_fsm:set_teams(jagiellonia, polonia).
scoreboard_fsm:start_game().
scoreboard_fsm:add_point(polonia, 2).


L = [{jagiellonia, 0}, {polonia, 0}].
State = dict:from_list(L).
Team = polonia.
N = 3.
dict:update(Team, fun(X) -> X + N end, State).
