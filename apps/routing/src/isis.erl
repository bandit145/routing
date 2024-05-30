-module(isis).
-behavior(gen_server).
-export([handle_cast/2, handle_call/3, init/1, start_link/1]).

-record(state, {net, hostname, socket, interface}).

-record(isis_hello, {circuit_type, sender_sys_id, holding_timer, pdu_len, prioritiy, desig_sys_id, protos_supported=[], area_addresses=[], ip_interface_addresses=[]}).

start_link(Args) ->
	gen_server:start_link(?MODULE, Args)

init(Args) ->
	{ok, #state{net=maps:get("net", Args), hostname=maps:get("hostname", Args), interface=maps:get("interface")}}.

handle_cast(Request, State) ->
	{noreply, State}.

handle_call(Request, From, State) ->
	{noreply, State}.

%%%
net_string_to_bits(Net) ->
	net_string_to_bits(string:split(Net, ".").

net_string_to_bits("", Accum) -> Accum.

net_string_to_bits(Net, Accum) ->
	case chars:length(Net) of
		2 -> {Int, _} = string:to_integer(Net),
		     <<Int:16>>,
		_ -> net_string_to_bits()


%%%
generate_ether_frame(SrcMac, DstMac, PayloadSize) ->
	Preamble = <<1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,01,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0,1,0>>,
	SFDByte = <<1,0,1,0,1,0,1,1>.

generate_hello(sys_id, area_addresses, ip_interface_addresses, holding_timer) ->
	LogicalLinkControlHeader = <<16#fe:8,16#fe:8,16#03:8>>,
	%15 is L1 Hello
	ISISHeader = <<83:8, 16#1b:8, 1:8, 0:8, 16#0f:8, 15:8, 1:8, 0:8, 0:8>>,


