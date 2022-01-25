-module(asset_manager_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, stop_child/1]).

-define(SHUTDOWN_TIMEOUT, timer:minutes(1)).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_one, 10, 1},
	  []}}.

-spec stop_child(atom()) -> ok.
stop_child(Name) ->
    _ = supervisor:terminate_child(?MODULE, Name),
    _ = supervisor:delete_child(?MODULE, Name),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
worker(Mod) ->
    {Mod, {Mod, start_link, []}, permanent, ?SHUTDOWN_TIMEOUT, worker, [Mod]}.

supervisor(Mod) ->
    supervisor(Mod, Mod).

%%     Children = [
%%        {lager, {gen_event, start_link, [{local, lager_event}]},
%%            permanent, 5000, worker, dynamic},
%%        {lager_handler_watcher_sup, {lager_handler_watcher_sup, start_link, []},
%%            permanent, 5000, supervisor, [lager_handler_watcher_sup]}],
supervisor(Name, Mod) ->
    {Name, {Mod, start_link, []}, permanent, infinity, supervisor, [Mod]}.

%%simple_supervisor(Mod) ->
%%    Name = list_to_atom(atom_to_list(Mod) ++ "_sup"),
%%    {Name, {ejabberd_tmp_sup, start_link, [Name, Mod]},
%%     permanent, infinity, supervisor, [ejabberd_tmp_sup]}.
