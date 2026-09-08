-module(routing_controller).
-behavior(gen_server).
-record(state, {instance_sup, interfaces, instances, supervisor}).
-export([init/1, start_link/1, handle_cast/2, handle_call/3]).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args, []).

init(Supervisor) ->
	gen_server:cast(self(), start),
	{ok, #state{instances=#{}, interfaces=#{}, supervisor=Supervisor}}.

handle_cast(start, State) ->
	{ok, {_, Pid, _, _}} = supervisor:which_child(State#state.supervisor, instance_sup),
	{noreply, State#state{instance_sup=Pid}}.

%%Just handle ISIS for now. diff instance difference here
handle_call({interfaces, Interfaces}, _, State) ->
	F = fun({Name, Config}, StateInterfaces) ->
			    case maps:is_key(Name, StateInterfaces) of
			    false -> {ok, Child} = supervisor:start_child(State#state.instance_sup, #{id => Name, start => {interface, start_link, [{Name, Config}]}}),
				InstanceName = "isis_" ++ binary_to_list(maps:get(<<"instance">>, maps:get(<<"isis">>, Config))),
				{ok, {_,Instance,_,_}} = supervisor:which_child(State#state.instance_sup, InstanceName),

				gen_server:cast(Child,{instance_update, Instance}),
				maps:put(Name, Child, StateInterfaces);
			    true -> StateInterfaces
			    end
	    end,
	NewInterfaces = lists:foldl(F,State#state.interfaces, maps:to_list(Interfaces)),
	{reply, {ok, nil} ,State#state{interfaces=NewInterfaces}};

handle_call({instances, Instances}, _, State) ->
	F = fun(X, StateInstances) ->
			    Name = unicode:characters_to_list(["isis_", maps:get(<<"name">>, X)]),
			    case maps:is_key(Name, StateInstances) of
			    false -> {ok, Child} = supervisor:start_child(State#state.instance_sup, #{id => Name, start => {isis, start_link, [X]}}),
				     maps:put(Name, Child, StateInstances);
			    true -> StateInstances
			    end
	    end,
	NewInstances = lists:foldl(F,State#state.instances, Instances),
	{reply, {ok, nil} ,State#state{instances=NewInstances}}.
