%%%-------------------------------------------------------------------
%% @doc routing top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(routing_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    io:format(user, "wat~n", []),
    {Idx, HwAddr} = find_interface("vboxnet5"),
    {ok, Socket} = socket:open(17, raw, 16#0100),
    ChildSpecs = [#{id => ospf_speaker, start => {ospfv3, start_link, [#{"socket" => Socket, "hostname" => "test", "ifindex" => Idx}]}}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions

find_interface(Name) ->
	{ok, Interfaces} = net:if_names(),
	{ok, InterfaceInfo} = inet:getifaddrs(),
	Idx = lists:last([Idx || {Idx, NetName} <- Interfaces, NetName =:= Name ]),
	HwAddr = lists:last([ list_to_binary(X) || {NetName, [_,_,_,_,_,_,{_, X}]} <- InterfaceInfo, NetName =:= Name]),
	{Idx, HwAddr}.
