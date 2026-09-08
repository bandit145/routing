-module(interface).
-include("routing.hrl").
-behavior(gen_server).
-export([handle_cast/2, handle_call/3, handle_info/2, init/1, start_link/1]).

-record(state, {name, socket, config, instance}).

start_link({Name, Config}) ->
	gen_server:start_link(?MODULE, {Name, Config}, []).

init({Name, Config}) ->

	%3 is all ethernet traffic
	{ok, Socket} = socket:open(17, 3, 3),
	gen_server:cast(self(), start),
	{ok, #state{name=Name, config=Config, socket=Socket}}.


handle_cast({instance_update, Instance}, State) ->
	gen_server:cast(Instance, {interface_register, self()}),
	{noreply, State#state{instance=Instance}};

handle_cast({send, Data}, State) when is_record(Data, isis_l1_hello) ->
	logger:debug("I would send ~p", [Data]),
	{noreply, State};

handle_cast(recv, State) ->
	{ok, Msg} = socket:recvmsg(State#state.socket, 1518, 1518, [], infinity),
	MsgData = lists:nth(1, maps:get(iov, Msg)),
	logger:debug("Frame length ~p~n", [length(binary_to_list(MsgData))]),
	{ok, Data} = packets:parse(MsgData),
	case {Data, State#state.instance} of
		{_, undefined} -> ok;
		{#ether_frame{data=#logical_link_control{data=PDU}}, Instance} -> gen_server:cast(Instance, PDU);
		_ -> logger:debug("Should not Be here"),
			ok 
	end,
	gen_server:cast(self(), recv),
	{noreply, State};
handle_cast(start, State) ->
	logger:debug("HUH"),

	F = fun({N, D}, Accum) ->
		M = lists:nth(1, [element(2, X) || X <:- D, element(1,X) =:= hwaddr]),
		case N =:= binary_to_list(State#state.name) of 
			true -> Accum ++ [M];
			false -> Accum
		end
	    end,
	{ok, Index} = net:if_name2index(binary_to_list(State#state.name)),
	{ok, Interfaces} = inet:getifaddrs(),
	MAC = list_to_binary(lists:nth(1, lists:foldl(F, [], Interfaces))),
	Proto = <<3:16>>,
	Data = <<Proto/binary, Index:32/native-signed-integer, 0:16, 0:8, 0:8, 0:64>>,
	ok = socket:bind(State#state.socket, #{family => 17, addr => Data}),
	gen_server:cast(self(), recv),
	{noreply, State}.

handle_info(Data, State) ->
	logger:debug("DATA ~p~n", [Data]),
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.


