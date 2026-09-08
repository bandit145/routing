-module(db).
-export([init/0]).
-include("routing.hrl").

init() -> 
	ets:new(routes_v6, [ordered_set]),
	ets:new(routes_v4, [ordered_set]),
	ets:new(isis_neighbors, [ordered_set]).

