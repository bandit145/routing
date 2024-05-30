%%%-------------------------------------------------------------------
%% @doc routing public API
%% @end
%%%-------------------------------------------------------------------

-module(routing_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    routing_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
