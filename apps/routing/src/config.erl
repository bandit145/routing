-module(config).
-behavior(gen_server).
-record(state, {old_config, config, routing_controller_pid, interface_manager_pid}).
-export([init/1, start_link/1, handle_cast/2, handle_call/3]).

start_link(_) ->
	gen_server:start_link(?MODULE, [],[]).

init(_) ->
	{ok, #state{}}.

handle_cast({file_update_data, Data}, State) ->
	Config = json:decode(Data),
	logger:info("~p~n", [Config]),
	ok = load_config(Config),
	{noreply, State#state{old_config=State#state.config, config=Config}}.

handle_call(_ , _, State) ->
	{noreply, State}.
load_config([]) ->
	ok;
load_config(Config) when is_map(Config) ->
	load_config(maps:to_list());
load_config([H|T], ) ->
	{ok, Res} = gen_server:call()
