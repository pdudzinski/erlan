-module(wi_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _StartArgs) ->
    wi_sup:start_link().

stop(_State) ->
    ok.

%erl -boot wi_release-1.0
%wi_server:create_account(1, "Jack").
%wi_account:start_trial(1).
%wi_account:request_all(1).
