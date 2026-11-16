-module(db).
-export([init/0]).
-include("routing.hrl").
-include_lib("eunit/include/eunit.hrl").

init() -> 
	ets:new(routes_v6, [ordered_set]),
	ets:new(routes_v4, [ordered_set]),
	ets:new(isis_neighbors, [ordered_set]).


insert_path(neighbor#{{System, }, Edges})

shortest_path(Address) ->


spf_test() ->
	?assert(


