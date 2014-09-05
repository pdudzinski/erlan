-module(curling_scoreboard_hw).

-export([set_teams/2, next_round/0, add_points/2, reset_board/0]).

set_teams(TeamA, TeamB) ->
    io:format('SCOREBOARD: ~s vs ~s~n', [TeamA, TeamB]).

next_round() ->
    io:format('SCOREBOARD: Lets move to the next round!~n').
    
add_points(Team, N) ->
    io:format('SCOREBOARD: Team ~s gained ~p points~n', [Team, N]).
    
reset_board() ->
    io:format('SCOREBOARD: Reseted~n~n').
