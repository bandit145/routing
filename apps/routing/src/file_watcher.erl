-module(file_watcher).
-behavior(gen_server).
-record(state, {modify_time, file, notify_pid, sup_pid}).
-export([init/1, start_link/1, handle_cast/2, handle_call/3]).
-include_lib("kernel/include/file.hrl").

start_link(Args) ->
	gen_server:start_link(?MODULE, Args,[]).

init({FileName, SupPid}) ->
	gen_server:cast(self(), start),
	gen_server:cast(self(), tick),


	{ok, #state{file=FileName, sup_pid=SupPid}}.

handle_cast(start, State) ->
	{ok, {_, NotifyPid, _, _}} = supervisor:which_child(State#state.sup_pid, config),
	{noreply, State#state{notify_pid=NotifyPid}};

handle_cast(tick, State) ->
	case file:read_file_info(State#state.file) of
	{ok, FileInfo} ->
			case FileInfo#file_info.mtime =/= State#state.modify_time of
				true ->
					ModifyTime = FileInfo#file_info.mtime,
					logger:info("File Changed"),
					{ok, Data} = file:read_file(State#state.file),
					gen_server:cast(State#state.notify_pid, {file_update_data, Data});
				false -> ModifyTime = State#state.modify_time
			end;
	{error, Reason} ->
			logger:error("Error with watched file ~s", [Reason]),
			ModifyTime = State#state.modify_time
	end,
	timer:sleep(1000),
	gen_server:cast(self(), tick),
	{noreply, State#state{modify_time=ModifyTime}}.

handle_call(_, _, State) ->
	{noreply, State}.
