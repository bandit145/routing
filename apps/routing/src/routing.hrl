
-define(ISIS_CIRCUIT_TYPE_L12, 16#03).
-define(ISIS_PDU_TYPE_L1_HELLO, 15).

-record(isis_pdu, {length, protocol_id_ext, id_length, pdu_type, version, max_area_addresses, circuit_type, source_id, holding_time, pdu_length, priority, lan_id, fields}).

-record(isis_field, {code, value}).
