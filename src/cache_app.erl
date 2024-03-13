-module(cache_app).
-behaviour(application).

-export([start/2, stop/1]).

-define(TABLE, table).

%-export([create/1, insert/3, insert/4, lookup/2]).

start(_StartTyp, _StartArgs) ->
Dispatch = cowboy_router:compile([
        {'_', [
            {"/api/cache_server", cache_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(cache_handler, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    cache_sup:start_link().

stop(_State) ->
    ok = cowboy:stop_listener(cache_handler).

%%====================================================================
%% application interface
%%====================================================================

%create(TableName) ->
%    cache_srv:create(TableName).

%insert(TableName, Key, Value) ->
%    cache_srv:insert(TableName, Key, Value).

%insert(TableName, Key, Value, TTL) ->
%    cache_srv:insert(TableName, Key, Value, TTL).

%lookup(TableName, Key) ->
%    cache_srv:lookup(TableName, Key).