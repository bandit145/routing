-module(routing_controller).
-behavior(gen_server).
-record(state, {isis_instances}).
-export([init/1, start_link/1, handle_cast/2, handle_call/3]).

start_link(_) ->
	gen_server:start_link(?MODULE, [], []).

init(_) ->
	{ok, #state{isis_instances=[]}}.

handle_cast(_, State) ->
	{noreply, State}.

handle_call(_, _, State) ->
	{noreply, State}.
