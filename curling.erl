-module(curling).

-export([start_link/2, set_teams/3, add_points/3, next_round/1]).

start_link(TeamA, TeamB) ->
    Pid = curling_events_handlers:start_link(),
    set_teams(Pid, TeamA, TeamB),
    {ok, Pid}.
    
set_teams(Pid, TeamA, TeamB) ->
    gen_event:notify(Pid, {set_teams, TeamA, TeamB}).
 
add_points(Pid, Team, N) ->
    gen_event:notify(Pid, {add_points, Team, N}).
 
next_round(Pid) ->
    gen_event:notify(Pid, next_round).


%% Subscribes the pid ToPid to the event feed.
%% The specific event handler for the newsfeed is
%% returned in case someone wants to leave
join_feed(Pid, ToPid) ->
    HandlerId = {curling_feed, make_ref()},
    %curling_feed is not written module to handle calls
    gen_event:add_handler(Pid, HandlerId, [ToPid]),
    HandlerId.
 
leave_feed(Pid, HandlerId) ->
    gen_event:delete_handler(Pid, HandlerId, leave_feed).


c(curling_scoreboard_hw).
c(curling_events_handlers).
c(curling).
Pid = curling:start_link("Pirates", "Scotsmen").
curling:add_points(Pid, "Scotsmen", 2).
