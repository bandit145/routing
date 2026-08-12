-module(isis).
-include("routing.hrl").
-behavior(gen_server).
-export([handle_cast/2, handle_call/3, init/1, start_link/1]).

-record(state, {net, hostname, socket, interface}).

-record(isis_hello, {circuit_type, sender_sys_id, holding_timer, pdu_len, prioritiy, desig_sys_id, protos_supported=[], area_addresses=[], ip_interface_addresses=[]}).


start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([Args]) ->
	ok = socket:bind(maps:get("socket", Args), #{family => packet, protocol => 16#0100, pkttype =>host, hatype => ether, ifindex => maps:get("ifindex", Args), addr => maps:get("hwaddr", Args)}),
	{ok, #state{net=maps:get("net", Args), hostname=maps:get("hostname", Args), socket=maps:get("socket", Args)}}.

handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.


