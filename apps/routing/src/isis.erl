-module(isis).
-behavior(gen_server).
-export([handle_cast/2, handle_call/3, init/1, start_link/1, net_string_to_bits/1, generate_hello/5]).

-record(state, {net, hostname, socket, interface}).

-record(isis_hello, {circuit_type, sender_sys_id, holding_timer, pdu_len, prioritiy, desig_sys_id, protos_supported=[], area_addresses=[], ip_interface_addresses=[]}).

-define(ISIS_CIRCUIT_TYPE_L12, 16#03).
-define(ISIS_PDU_TYPE_L1_HELLO, 15).

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([Args]) ->
	ok = socket:bind(maps:get("socket", Args), #{family => packet, protocol => 16#0100, pkttype =>host, hatype => ether, ifindex => maps:get("ifindex", Args), addr => maps:get("hwaddr", Args)}),
	{ok, #state{net=maps:get("net", Args), hostname=maps:get("hostname", Args), socket=maps:get("socket", Args)}}.

handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

%%%
net_string_to_bits(Net) ->
	net_string_to_bits(string:split(Net, ".", all), []).

net_string_to_bits([], Accum) -> Accum;

net_string_to_bits([H |T], Accum) when length(H) =:= 2 ->
	{Int, _} = string:to_integer(string:left(H,1)),
	{Int2, _} = string:to_integer(string:right(H,1)),
	net_string_to_bits(T, [<<Int:8>>, <<Int2:8>>] ++ Accum);

net_string_to_bits([H |T], Accum) when length(H) =:= 4 ->
	LeftOctet = string:left(H,2),
	RightOctet = string:right(H,2),
	{Int, _} = string:to_integer(string:left(LeftOctet, 1)),
	{Int2, _} = string:to_integer(string:right(LeftOctet,1)),
	{Int3, _} = string:to_integer(string:left(RightOctet, 1)),
	{Int4, _} = string:to_integer(string:right(RightOctet, 1)),
	net_string_to_bits(T, [<<Int:8>>, <<Int2:8>>, <<Int3:8>>, <<Int4:8>>] ++ Accum).


generate_hello(SysId, AreaAddresses, IPInterfaceAddress, HoldTimer, Accum) ->
	LogicalLinkControlHeader = <<16#fe:16,16#fe:16,16#03:16>>,
	%15 is L1 Hello
	ISISHeader = <<16#83:16, 16#1b:16, 16#01:16, 16#00:16, 0:8, ?ISIS_PDU_TYPE_L1_HELLO:8, 16#01:16, 16#00:16, 16#00:16>>,
	[LogicalLinkControlHeader, ISISHeader].


