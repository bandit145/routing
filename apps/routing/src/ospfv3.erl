-module(ospfv3).
-behavior(gen_server).

-export([handle_cast/2,handle_call/3,handle_info/3,start_link/1, init/1,terminate/2,code_change/3]).

-record(state, {router_id, area_id, hello_interval=10, router_dead_interval=40, router_priority=0, instance_id, neighbors=[], if_addr, socket}).

start_link(Args) ->
	gen_server:start_link(?MODULE, [Args], []).

init([Args]) ->
	IfAddr = maps:get("ifaddr", Args),
	IfIndex = maps:get("ifindex", Args),
	HwAddr = maps:get("hwaddr", Args),
	MaBin =  <<65282:16,0:16,0:16,0:16,0:16,0:16,0:16,5:16>>,
	IndexBin = <<IfIndex:64/native>>,
	{ok, AllOspfSpf} = inet:parse_ipv6_address("ff02::5"),
	{ok, Socket} = socket:open(inet6, raw, ipv6),
	ok = socket:setopt_native(Socket, {ipv6, 20}, <<MaBin/binary, IndexBin/binary>>),
	ok = socket:bind(Socket, #{family => inet6, port =>0, addr => AllOspfSpf, scope_id => IfIndex}),
	gen_server:cast(self(), start),
	{ok, #state{router_id=maps:get("router_id", Args), socket=Socket, if_addr=IfAddr, area_id=maps:get("area_id", Args)}}.
	
handle_cast(start, State) ->
	socket:send(State#state.socket, generate_hello(State#state.router_id, State#state.area_id, State#state.hello_interval, State#state.router_dead_interval, State#state.router_priority, 0, {0,0,0,1} ,0)),
	{ok, State}.
handle_call(_,_,_) ->
	ok.
handle_info(_,_,_) ->
	ok.

terminate(Reason, State) ->
	ok.
code_change(_,_,_) ->
	ok.

%%%ospfv3 specific functions

generate_packet_checksum(Packet) ->
	bnot(lists:sum(generate_packet_checksum(Packet, []))) +1.

generate_packet_checksum(<<>>, Accum) ->
	Accum;
generate_packet_checksum(Packet, Accum) when bit_size(Packet) < 16 ->
	ActualSize = bit_size(Packet),
	generate_packet_checksum(<<Packet, 0:(16-ActualSize)>>, Accum);
generate_packet_checksum(<<Word:16, Packet/binary>>, Accum) ->
	generate_packet_checksum(Packet, [bnot(Word)+1] + Accum).


generate_hello(RouterId, AreaId, HelloInterval, RouterDeadInterval, RouterPriority, InstanceId, SourceRouter, InterfaceId) ->
	Version = <<16#3:8>>,
	MsgType = <<16#1:8>>,
	{SrcRouterOctet1,SrcRouterOctet2, SrcRouterOctet3, SrcRouterOctet4} = RouterId,
	{AreaIdOctet1, AreaIdOctet2, AreaIdOctet3, AreaIdOctet4} = AreaId,
	Hello = << InstanceId:32, RouterPriority:8,0,0,0,0,1,0,0,1,1, HelloInterval:16, RouterDeadInterval:16, 0:8,0:8,0:8,0:8, 0:8,0:8,0:8,0:8, 0:8,0:8,0:8,0:8>>,
	Header1 = << Version/binary, MsgType/binary, (byte_size(Hello)+24):8, SrcRouterOctet1:8, SrcRouterOctet2:8, SrcRouterOctet3:8, SrcRouterOctet4:8, AreaIdOctet1:8, AreaIdOctet2:8, AreaIdOctet3:8, AreaIdOctet4:8>>,
	Header2 = <<InstanceId:8, 0:8>>,
	<<Header1/binary, (generate_packet_checksum(<<Header1/binary, Header2/binary, Hello/binary>>)):16, Header2/binary, Hello/binary>>.
