
-module(local_db_worker).

-behaviour(gen_server).


-include("logger.hrl").
-include("db.hrl").

-export([start_link/0]).
% -export([start/1, update_app_config/1, new_app_handler/2, get_auth_url/0]).
%% gen_server callbacks
-export([init/1, handle_cast/2, handle_info/2, handle_call/3, terminate/2,
         code_change/3]).


-export([check_auth/2, register_auth/3]).


-define(SERVER, ?MODULE).

-record(state, { }).




start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,  [], []).



init([]) ->
    PidFilename = application:get_env(asset_manager, db_file, "stage/auth.db"),
    {ok, _Ref} = dets:open_file(?AMS_LOCAL_DB_DETS, [{access, read_write}, {auto_save, infinity} , {file, PidFilename}, {type, set}]),
    % CacheOpts = [named_table , set, public, {read_concurrency, true}],
    % ets:new(?ETS_MQTT_APP_CONFIG, CacheOpts),
    % {ok, RE} = re:compile("[^a-zA-Z0-9@_+#/\-]"),
    % ets:insert(?ETS_MQTT_APP_CONFIG, {?CHECK_TOPIC_VALID_RE, RE}),
    % CacheOpts1 = [named_table, set, public, {read_concurrency, true},{write_concurrency, true}],
    % ets:new(?ETS_MQTT_METRICS, CacheOpts1),
    {ok, #state{}}.



% % %%%===================================================================
% % %%% Export functions
% % %%%===================================================================
-spec check_auth(binary(), binary()) -> {ok, term()} | notfound.
check_auth(Username, Password) ->
    case dets:lookup(?AMS_LOCAL_DB_DETS, {username, Username, Password}) of
        [{_, Info}] -> {ok, Info};
        [] -> notfound;
        {error, Reason} -> ?WARNING_MSG("NotFound Key(~p): ~p", [{Username, Password}, Reason ]), notfound
    end.


-spec register_auth(binary(), binary(), term()) -> {ok, term()} | notfound.
register_auth(Username, Password, Info) ->
    try
        case check_auth(Username, Password) of
            notfound ->
                BUUID0 = erlang:list_to_binary(uuid:to_string(uuid:uuid1())),
                insert_auth_to_db(Username, Password, BUUID0, Info),
                insert_uuid_userinfo_to_db(Username, Password, BUUID0);
            {ok, _} -> ok
        end
    catch
        _:Err:Strack ->
            ?ERROR_MSG("insert Auth(~p:~p): ~p~n~p", [ Username, Password, Err, Strack]),
            {error, Err}
    end.




% % %%%===================================================================
% % %%% Private functions
% % %%%===================================================================

insert_auth_to_db(Username, Password, BUUID, Info) ->
    case dets:insert(?AMS_LOCAL_DB_DETS, {{username, Username, Password}, Info#{<<"uuid">> => BUUID, <<"token">> => BUUID}}) of
        {error, Reason} -> 
            ?WARNING_MSG("NotFound Key(~p): ~p", [Reason, {Username, Password}]), 
            err(Reason);
        ok -> ok
    end.


insert_uuid_userinfo_to_db(Username, Password, BUUID) ->
    case dets:insert(?AMS_LOCAL_DB_DETS, {{uuid, BUUID}, Username, Password}) of
        {error, Reason} -> 
            ?WARNING_MSG("NotFound Key(~p): ~p", [Reason, {Username, Password}]), 
            err(Reason);
        ok -> ok
    end.


% % %%%===================================================================
% % %%% Cache functions
% % %%%===================================================================
handle_cast(Msg, State) ->
    ?WARNING_MSG("[app_manager] Unexpected cast: ~p", [Msg]),
    noreply(State).


handle_info(Info, State) ->
    ?WARNING_MSG("[app_manager] Unexpected info: ~p", [Info]),
    noreply(State).


handle_call(Info, _From, State) ->
    ?WARNING_MSG("[app_manager] Unexpected call: ~p", [Info]),
    reply(Info, State).


terminate(Reason, _State) ->
    ?WARNING_MSG("[app_manager] terminate reason: ~p", [Reason]),
    % easemob_metrics_statistic:rts_deregister(AppId),
    % ets:delete(? AppId),
    ok.

code_change(_OldVsn, State, _Extra) ->
    % {ok, State, hibernate}.
    {ok, State, hibernate}.







-compile({inline, [stop/1, noreply/1, reply/2]}).

stop(State) ->
    {stop, normal, State}.

noreply(State) ->
    {noreply, State, hibernate}.

reply(Reply, State) ->
    {reply, Reply, State, hibernate}.


err(Err) ->
    erlang:error({error, Err}).