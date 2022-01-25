%%%-------------------------------------------------------------------
%%% @author lijiahao <>
%%% @copyright (C) 2021, lijiahao
%%% @doc
%%%
%%% @end
%%% Created : 10 April 2021 by lijiahao  <>
%%%-------------------------------------------------------------------
-module(native_http_api).

%% Cowboy http callbacks
-export([init/2]).

-include("logger.hrl").
-include("db.hrl").



-define(HTTP_HEADER, 
        #{<<"content-type">> => <<"text/json; charset=utf-8">>
        , <<"Access-Control-Allow-Origin">> => <<"*">>}).






init(Req0, Opts) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    _Headers = cowboy_req:headers(Req0),
    Res = dispatch(Method, Path, Req0),
    {ok, Res, Opts}.



dispatch(<<"POST">>, <<"/openapi/login">>,  Req) ->
    ?DEBUG("HTTP ↑ /openapi/login.", []),
    try
        {ok, DataBin, Req1} = cowboy_req:read_body(Req),
        ?DEBUG("LJH recv one req ~p", [DataBin]),
        #{<<"user">> := Username, <<"password">> := Password} = jiffy:decode(DataBin, [return_maps]),
        Reply = case local_db_worker:check_auth(Username, Password) of
            {ok, Info} -> #{<<"status">> => 200, <<"msg">> => <<"success">>, <<"data">> => Info};
            notfound -> #{<<"msg">> => <<"Notfound Username">>}
        end,
        Reply0 = jiffy:encode(Reply),
        cowboy_req:reply(200, ?HTTP_HEADER, Reply0, Req1)
    catch
        _:Err -> 
            ?DEBUG("failed to parse json params : ~p", [Err]),
            Reply1 = #{<<"msg">> => <<"bad-json">>},
            Reply2 = jiffy:encode(Reply1),
            cowboy_req:reply(400, ?HTTP_HEADER, Reply2, Req)
    end;




dispatch(<<"POST">>, <<"/openapi/register">>,  Req) ->
    ?DEBUG("HTTP ↑ /openapi/register.", []),
    try
        {ok, DataBin, Req1} = cowboy_req:read_body(Req),
        #{<<"user">> := Username, <<"password">> := Password, <<"realname">> := Realname, <<"roles">> := Roles} = 
            jiffy:decode(DataBin, [return_maps]),
        Reply = case local_db_worker:register_auth(Username, Password, #{<<"realname">> => Realname, <<"roles">> => Roles}) of
            ok -> #{<<"status">> => 200, <<"msg">> => <<"success">>};
            {error, Err} -> #{<<"msg">> => Err}
        end,
        Reply1 = jiffy:encode(Reply),
        cowboy_req:reply(200, ?HTTP_HEADER, Reply1, Req1)
    catch
        _:Err1 ->
            ?DEBUG("failed to parse json params : ~p", [Err1]),
            Reply2 = #{<<"msg">> => <<"bad-json">>},
            Reply3 = jiffy:encode(Reply2),
            cowboy_req:reply(400, ?HTTP_HEADER, Reply3, Req)
    end;




dispatch(Method, Path, Req) ->
    ?DEBUG("[~p] Method not allowed: [~p]", [Method, Path]),
    %% Method not allowed.
    cowboy_req:reply(405, Req).





% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% %%% internal api
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%




-spec err(term()) -> ok.
err(Reason) ->
    erlang:error(Reason).