-module(wi_accounts_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Id, Name) ->
    supervisor:start_child(?MODULE, [Id, Name]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
init([]) ->
    {ok, {{simple_one_for_one, 5, 10},
          [?CHILD(wi_account, wi_account, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
