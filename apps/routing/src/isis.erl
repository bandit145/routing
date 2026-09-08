-module(isis).
-include("routing.hrl").
-behavior(gen_server).
-export([handle_cast/2, handle_info/2, handle_call/3, init/1, start_link/1]).

-record(state, {area, system_id, hostname, interfaces=[], neighbors, ticker, holding_timer, designated_is}).

-record(timer, {name, last_time, interval}).

start_link(Config) ->
	gen_server:start_link(?MODULE, Config, []).

init(Config) ->
	{ok, T} = timer:send_interval(1000, self(), tick),
	{ok, #state{ticker=T}}.

handle_cast(Data, State) when is_record(Data, isis_l1_hello) ->
	logger:debug("GOT HELLO~n"),
	{noreply, State};

handle_cast({interface_register, Interface}, State) ->
	{noreply, State#state{interfaces=State#state.interfaces ++ [Interface]}};

handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

handle_info(tick, State) ->
	[gen_server:cast(X, {send,#isis_l1_hello{circuit_type=16#1, system_id=State#state.system_id, holding_timer=State#state.holding_timer, priority=64, designated_is=State#state.designated_is}}) || X <:- State#state.interfaces],
	{noreply, State}.
