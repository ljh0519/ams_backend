%%%-------------------------------------------------------------------
%%% @author 李嘉豪 <>
%%% @copyright (C) 2021, 李嘉豪
%%% @doc
%%%
%%% @end
%%% Created : 10 April 2021 by 李嘉豪  <>
%%%-------------------------------------------------------------------
%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(local_db_sup).

-behaviour(supervisor).
%% Supervisor callbacks
-export([start_link/0, init/1]).

-include("logger.hrl").
-include("db.hrl").

% -define(APP_LISTS, app_lists).


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================
-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init([]) ->
    Spec = {local_db_worker, {local_db_worker, start_link, []}, permanent, infinity, worker, [local_db_worker]},
    {ok, {{one_for_one, 10, 1}, [Spec]}}.