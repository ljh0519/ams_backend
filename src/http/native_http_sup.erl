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
-module(native_http_sup).

-behaviour(supervisor).

%% API.
-export([start_link/0]).
%% supervisor.
-export([init/1]).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [],
    {ok, {{one_for_one, 10, 1}, Procs}}.