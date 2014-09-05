-module(timeout).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2, handle_info/2,
         terminate/2, code_change/3, handle_call/3]).

-define(SERVER, ?MODULE).

%When we add TimeOut to init function
%Server will send timeout message aftex X seconds
%Which will be handled in handle_info
%Pid ! message is also handled there

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
    
init(_Args) ->
    io:format('Go baby! ~n'),
    {ok, initial_state, 3000}.
    
handle_call(_Message, _From, State) ->
    {noreply, State}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    io:format('Server timeouted? ~n'),
    {noreply, State, 10000}.
  
terminate(_Reason, _State) ->
    io:format('Server terminated! ~n'),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
