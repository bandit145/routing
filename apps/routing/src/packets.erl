-module(packets).
-include("routing.hrl").
-export([parse/1, encode/2]).
-define(ISISProtocolDiscriminator, 16#83).

encode(ethernet2, {Src, Dest, Length}) ->
	<<Src:48, Dest:46, Length:8>>;

encode(isis, {Src, Dest, PDU}) ->
	%%Static logical link control header (don't need anything dynamic here)
	case element(1, PDU) of
		isis_l1_hello -> PDUType = ?ISISL1HelloPDU;
			_ -> PDUType = 255
	end,
	{Length, PDU} = encode_isis_pdu(PDUType, PDU),
	LogicalLinkControl = <<177:8, 177:8, 16#3:8>>,
	ISISHeader = <<16#83:8, 27:8, 0:8, 0:3, PDUType:5, 1:8, 0:8, 0:8>>,
	PDULength = Length + length(binary_to_list(LogicalLinkControl))/8 + length(binary_to_list(ISISHeader))/8,
	Ether = encode(ethernet2, {Src, Dest, Length}),
	<<Ether, LogicalLinkControl, ISISHeader, PDU>>.


encode_isis_pdu(?ISISL1HelloPDU, #isis_l1_hello{circuit_type=CircuitType, system_id=SystemID, holding_timer=HoldingTimer, priority=Priority, designated_is=DesignatedIS, fields=Fields}) ->
	%Static sized fields (circtype/length/)
	Fields = encode_isis_fields(Fields),
	PDULength = 5 + 8 + length(binary_to_list(SystemID))/8 + length(binary_to_list(DesignatedIS))/8 + length(binary_to_list(Fields))/8,
	<<CircuitType:2, 0:6, SystemID, HoldingTimer:2, PDULength:16, Priority:8, DesignatedIS, Fields>>.

encode_isis_fields([]) ->
	<<>>;

encode_isis_fields([#isis_field{type=Type, value=Value}|T]) ->
	Length = length(binary_to_list(Value))/8,
	NextField = encode_isis_fields(T),
	<<Type:8, Length:8, Value, NextField>>.



parse_isis(?ISISL1HelloPDU, <<CircuitType:8/integer, SenderSystemID:48/bits, HoldingTimer:16/integer, PDULength:16/integer, Priority:8/integer, DesignatedIS:56/bits, Data/binary>>) ->
	#isis_l1_hello{circuit_type=CircuitType, system_id=SenderSystemID, holding_timer=HoldingTimer, pdu_length=PDULength, priority=Priority, designated_is=DesignatedIS, fields=parse_isis_fields([], Data)}.

parse_isis_fields(Accum, <<>>) ->
	Accum;
parse_isis_fields(Accum, <<?ISISIPInterfaceAddressField, 4, Data/binary>>) ->
	<<Value:32/bits, NewData/binary>> = Data,
	parse_isis_fields(Accum ++ [#isis_field{type=?ISISIPInterfaceAddressField, value=Value}], NewData);
parse_isis_fields(Accum, <<?ISISIPInterfaceAddressField, 16, Data/binary>>) ->
	<<Value:128/bits, NewData/binary>> = Data,
	parse_isis_fields(Accum ++ [#isis_field{type=?ISISIPInterfaceAddressField, value=Value}], NewData);
parse_isis_fields(Accum, <<?ISISAreaAddressesField, Length/integer, Data/binary>>) ->
	BitLen = Length *8,
	<<Value:BitLen/bits, NewData/binary>> = Data,
	parse_isis_fields(Accum ++ [#isis_field{type=?ISISAreaAddressesField, value=Value}], NewData);


parse_isis_fields(Accum, <<?ISISPaddingField, Length/integer, Data/binary>>) -> 
	BitLen = Length*8,
	<<Value:BitLen, NewData/binary>> = Data,
	parse_isis_fields(Accum, NewData);
parse_isis_fields(Accum, <<Type/integer, Length/integer, Data/binary>>) ->
	BitLen = Length * 8,
	<<Value:BitLen, NewData/binary>> = Data,
	parse_isis_fields(Accum ++ [#isis_field{type=Type, value=Value}], NewData).


%%ISIS
parse(<<?ISISProtocolDiscriminator, Length/integer, VersionProtoIDExtension/integer, IDLength/integer, _:3, PDUType:5/integer, Version/integer, Reserved/integer, MaxiumumAreaAddresses/integer, Data/binary>>) ->
	parse_isis(PDUType, Data);

parse(<<254,254, Command:6/integer, FrameType:2/integer, Data/binary>>) ->
	SAP = <<1,1,1,1,1,1,1>>,
	IG = <<0>>,
	logger:debug("Logical Link control ~p~n", [Data]),
	#logical_link_control{dsap=#destination_service_access_point{sap=SAP, ig=IG}, ssap=#source_service_access_point{sap=SAP, cr=IG}, control_field=#control_field{command=Command, frame_type=FrameType}, data=parse(Data)};

%%Ethernet
parse(<<Dest:48/bits, Src:48/bits, Length:16/integer, Data/binary>>) ->
	logger:debug("FRAME DATA ~p~n",[Data]),
	{ok, #ether_frame{dest=Dest, src=Src, length=Length, data=parse(Data)}};

parse(Data) ->
	logger:debug("Unknown data~n~p", [Data]),
	ok.



