-module(cache_handler).
-export([init/2]).

-behavior(cowboy_handler).

-define(TABLE, table).

init(Req0=#{method := <<"POST">>}, State) ->
    {ok, Json, _R} = cowboy_req:read_body(Req0),
    DataFromJson = jsx:decode(Json,[return_maps]),
    ReqResult = jsx:encode(request_handler(DataFromJson)),
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, ReqResult, Req0),
    {ok, Req, State};
init(Req0, State) ->
    Req = cowboy_req:reply(405, #{
        <<"allow">> => <<"POST">>
    }, Req0),
    {ok, Req, State}.


request_handler(#{<<"action">> := <<"insert">>, <<"key">> := Key, <<"value">> := Value, <<"ttl">> := TTL}) ->
    cache_srv:insert(?TABLE, Key, Value, TTL),
    [{<<"result">>, <<"ok">>}];

request_handler(#{<<"action">> := <<"insert">>, <<"key">> := Key, <<"value">> := Value}) ->
    cache_srv:insert(?TABLE, Key, Value),
    [{<<"result">>, <<"ok">>}];

request_handler(#{<<"action">> := <<"lookup">>, <<"key">> := Key}) ->
    case cache_srv:lookup(?TABLE, Key) of
        undefined ->
            [{<<"result">>, <<"not found">>}];
        Value ->
            [{<<"result">>, Value}]
    end;

request_handler(#{<<"action">> := <<"lookup_by_date">>, <<"date_from">> := DateFrom, <<"date_to">> := DateTo}) ->
    case cache_srv:lookup_by_date(DateFrom, DateTo) of
        false ->
            [{<<"result">>, <<"not found">>}];
        true ->
            [{<<"result">>, <<"exists">>}]
    end.
