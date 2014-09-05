-module(chat_user_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    
start_child(User) ->
    supervisor:start_child(?MODULE, [User]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(chat_client_fsm, worker)]}}.
