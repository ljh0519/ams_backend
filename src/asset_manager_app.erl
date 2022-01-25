
-module(asset_manager_app).

-behaviour(application).

-export([start/2, prep_stop/1, stop/1]).

-include("logger.hrl").

%%%
%%% Application API
%%%

start(normal, _Args) ->
	erlang:process_flag(min_bin_vheap_size, 1024*1024),
	erlang:process_flag(min_heap_size, 1024*1024),
	check_tmp_dir(),
	write_pid_file(),
	native_http_server:start(),
	local_db_sup:start_link(),
	case asset_manager_sup:start_link() of
		{ok, SupPid} -> {ok, SupPid};
		Err ->
			?CRITICAL_MSG("Failed to start threeq application: ~p", [Err]),
			erlang:halt(1, [{flush, true}])
	end;
	

start(_, _) ->
    {error, badarg}.



%% Prepare the application for termination.
%% This function is called when an application is about to be stopped,
%% before shutting down the processes of the application.
prep_stop(State) ->
    % ejabberd_sm:stop(),
    % ejabberd_service:stop(),
    % ejabberd_s2s:stop(),
    State.

%% All the processes were killed when this function is called
stop(_State) ->
    ?INFO_MSG("ejabberd ~ts is stopped in the node ~p",
	      [ejabberd_option:version(), node()]),
    delete_pid_file().

%%%
%%% Internal functions
%%%

%% If ejabberd is running on some Windows machine, get nameservers and add to Erlang
% maybe_add_nameservers() ->
%     case os:type() of
% 	{win32, _} -> add_windows_nameservers();
% 	_ -> ok
%     end.

% add_windows_nameservers() ->
%     IPTs = win32_dns:get_nameservers(),
%     ?INFO_MSG("Adding machine's DNS IPs to Erlang system:~n~p", [IPTs]),
%     lists:foreach(fun(IPT) -> inet_db:add_ns(IPT) end, IPTs).

%%%
%%% PID file
%%%

check_tmp_dir() ->
    TmpDir = application:get_env(asset_manager, tmp_dir, <<"stage">>),
	file:make_dir(TmpDir).



write_pid_file() ->
    PidFilename = application:get_env(asset_manager, pid_file, <<"stage/pid.file">>),
	write_pid_file(os:getpid(), PidFilename).

write_pid_file(Pid, PidFilename) ->
    case file:write_file(PidFilename, io_lib:format("~ts~n", [Pid]), [write]) of
	ok ->
	    ok;
	{error, Reason} = Err ->
	    ?CRITICAL_MSG("Cannot write PID file ~ts: ~ts",
			  [PidFilename, file:format_error(Reason)]),
	    throw({?MODULE, Err})
    end.

delete_pid_file() ->
    PidFilename = application:get_env(asset_manager, pid_file, <<"stage/pid.file">>),
	file:delete(PidFilename).

