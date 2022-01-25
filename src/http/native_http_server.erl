%%%-------------------------------------------------------------------
%%% @author 李嘉豪 <>
%%% @copyright (C) 2021, 李嘉豪
%%% @doc
%%%
%%% @end
%%% Created : 10 April 2021 by 李嘉豪  <>
%%%-------------------------------------------------------------------
-module(native_http_server).

-export([start/0, start/1, stop/1]).

% -export([get_http_api_bind_addr/0]).

-include("logger.hrl").


-define(HTTP_API_SERVER, api_http_server).


start() ->
    start([]).

start(_Options) ->
    ensure_all_start(),
    case whereis(native_http_sup) of
        undefined ->
            start_api_http_server(),
            native_http_sup:start_link();
        _ ->
            ok
    end.


stop(_State) ->
    stop_api_http_server().



%%%%%%%%%%%%%%%%%%%%%%%
%%internal function
%%%%%%%%%%%%%%%%%%%%%%%

ensure_all_start() ->
    DepApps = [ranch, crypto, cowboy],
    [ensure_all_start(App) || App <- DepApps].

ensure_all_start(App) ->
    start_ok(App, application:start(App)).

start_ok(_App, ok) ->
    ok;
start_ok(_App, {error, {already_started, _App}}) ->
    ok;
start_ok(App, {error, {not_started, Dep}}) ->
    ok = ensure_all_start(Dep),
    ensure_all_start(App);
start_ok(App, {error, Reason}) ->
    ?ERROR_MSG("fail to start app: ~p, reason: ~p", [App, Reason]).

% init_state(CallBack, State) ->
%     #service{
%              callback = CallBack,
%              state = State
%     }.



start_api_http_server() ->
    Port = application:get_env(asset_manager, http_api_port, 8080),
    Dispatch = cowboy_router:compile([{'_', [{"/openapi/[...]", native_http_api, []}]}]),
    case cowboy:start_clear(?HTTP_API_SERVER, [{port, Port}], #{env => #{dispatch => Dispatch}}) of
        {ok, _} -> ok;
        {error,{already_started, _}} -> ok
    end.

stop_api_http_server() ->
    ok = cowboy:stop_listener(?HTTP_API_SERVER).




% -spec get_http_api_bind_addr() -> {inet:ip_address(), inet:port_number()} | {undefined, undefined}.
% get_http_api_bind_addr() ->
%     ranch:get_addr(?EM_HTTP_API_SERVER).