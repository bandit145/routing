-record(ether_frame, {dest, src, length, data}).
-record(destination_service_access_point, {sap, ig}).
-record(source_service_access_point, {sap, cr}).
-record(control_field, {command, frame_type}).
-record(logical_link_control, {dsap, ssap, control_field, data}).
-record(isis_l1_hello, {circuit_type, system_id, holding_timer, pdu_length, priority, designated_is, fields}).
-record(isis_field, {type, value}).

%%IS-IS Types
-define(ISISProtocolsSupportedField, 129).
-define(ISISAreaAddressesField,1).
-define(ISISIPInterfaceAddressField, 132).
-define(ISISPaddingField, 8).
-define(ISISL1HelloPDU, 15).

%%ERTS table records
-record(route, {priority, address, prefix_length, instance, interface,protocol}).
-record(neighbor, {system, interface}).

