-module(config).
-behavior(gen_server).
-record(state, {old_config, config, routing_controller_pid, supervisor}).
-export([init/1, start_link/1, handle_cast/2, handle_call/3]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args,[]).

init(Supervisor) ->
	gen_server:cast(self(), start),
	{ok, #state{supervisor=Supervisor}}.

handle_cast(start, State) -> 
	{ok, {_, Pid, _, _}} = supervisor:which_child(State#state.supervisor, routing_controller),
	{noreply, State#state{routing_controller_pid=Pid}};

handle_cast({file_update_data, Data}, State) ->
	Config = json:decode(Data),
	logger:info("~p~n", [Config]),
	ok = load_config(Config, State#state.routing_controller_pid),
	{noreply, State#state{old_config=State#state.config, config=Config}}.

handle_call(_ , _, State) ->
	{noreply, State}.
load_config(Config, Pid) when is_map(Config) ->
	{ok, _} = gen_server:call(Pid, {instances, maps:get(<<"isis">>, Config)}),
	{ok, _} = gen_server:call(Pid, {interfaces, maps:get(<<"interfaces">>, Config)}),
	ok.
