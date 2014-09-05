-module(wi_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).
-define(CHILD(Id), {Id, {Id, start_link,[]}, permanent, 2000, worker, [Id]}).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    {ok,{{one_for_one,5,10},
         [?CHILD(wi_server),
          ?CHILD(worker_request_all),
          {wi_accounts_sup, {wi_accounts_sup, start_link, []},
          permanent, 2000, supervisor, []}]}}.
