-module(cache_srv).
-behaviour(gen_server).

-define(TABLE, table).

%% API
-export([start_link/0,
        insert/3,
        insert/4,
        lookup/2,
        lookup_by_date/2
]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

%%====================================================================
%% API Functions
%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

insert(TableName, Key, Value) ->
    gen_server:call(?MODULE, {insert, TableName, Key, Value}).

insert(TableName, Key, Value, TTL) ->
    gen_server:call(?MODULE, {insert_ttl, TableName, Key, Value, TTL}).

lookup(TableName, Key) ->
    gen_server:call(?MODULE, {lookup, TableName, Key}).

lookup_by_date(DateFrom, DateTo) ->
    gen_server:call(?MODULE, {lookup_by_date, DateFrom, DateTo}).

%%====================================================================
%% gen_server Functions
%%====================================================================
init(_Args) ->
    CreationTimeTable = ets:new(creation_times, [named_table, set, public]),
    {ok, #{creation_time_table => CreationTimeTable}}.

handle_call({insert, TableName, Key, Value}, _From, State=#{creation_time_table := CreationTimeTable}) ->
    CurrentTime = calendar:universal_time(),
    TableId = ets:new(TableName, [named_table, set, private]),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds(CurrentTime),
    true = ets:insert(CreationTimeTable, {creation_time, {TableName, CurrentTimeSeconds}}),
    NewState = State#{TableName => TableId},
    io:format("Table: ~p successfully created with ID: ~p ~n", [TableName, TableId]),
    true = ets:insert(TableId, {Key, Value}),
    io:format("Table ~p successfully added ~p and ~p.~n", [TableName, Key, Value]),
    {reply, ok, NewState};

handle_call({insert_ttl, TableName, Key, Value, TTL}, _From, State=#{creation_time_table := CreationTimeTable}) ->
    CurrentTime = calendar:universal_time(),
    TableId = ets:new(TableName, [named_table, set, private]),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds(CurrentTime),
    true = ets:insert(CreationTimeTable, {creation_time, {TableName, CurrentTimeSeconds}}),
    NewState = State#{TableName => TableId},
    io:format("Table: ~p successfully created with ID: ~p ~n", [TableName, TableId]),
    {Date, Time} = calendar:universal_time(),
    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
    ExpiryTimeSeconds = CurrentTimeSeconds + TTL,
    true = ets:insert(TableId, {Key, Value, ExpiryTimeSeconds}),
    io:format("Table ~p successfully added ~p and ~p.~n", [TableName, Key, Value]),
    {reply, ok, NewState};

handle_call({lookup, TableName, Key}, _From, State) ->
    case maps:find(TableName, State) of
        {ok, TableId} ->
            case ets:lookup(TableId, Key) of
                [] ->
                    {reply, undefined, State};
                [{_, Value}] ->
                    {reply, Value, State};
                [{_, Value, ExpiryTimeSeconds}] ->
                    {Date, Time} = calendar:universal_time(),
                    CurrentTimeSeconds = calendar:datetime_to_gregorian_seconds({Date, Time}),
                    Reply = if CurrentTimeSeconds > ExpiryTimeSeconds ->
                        undefined;
                                true ->
                                    Value
                            end,
                    {reply, Reply, State}
            end;
        error ->
            {reply, undefined, State}
    end;

handle_call({lookup_by_date, DateFrom, DateTo}, _From, State) ->
    CreationTimesTable = get_creation_time(?TABLE),
    Reply = is_within_range(CreationTimesTable, DateFrom, DateTo),
    {reply, Reply, State};

handle_call({lookup}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(_Cast, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%====================================================================
%% Internal Functions
%%====================================================================

is_within_range(Timestamp, DateFrom, DateTo) ->
    FromSeconds = parse_datetime(DateFrom),
    ToSeconds = parse_datetime(DateTo),
    case Timestamp < FromSeconds of
        true ->
            false;
        false ->
            case Timestamp > ToSeconds of
                true ->
                    false;
                false ->
                    true
            end
    end.

format_datetime(Data) ->
    calendar:datetime_to_gregorian_seconds(Data).

parse_datetime(Str) ->
    [DateStr, TimeStr] = string:split(Str, " ", all),
    [Year, Month, Day] = [binary_to_integer(X) || X <- binary:split(DateStr, <<"/">>, [global])],
    [Hour, Minute, Second] = [binary_to_integer(X) || X <- binary:split(TimeStr, <<":">>, [global])],
    format_datetime({{Year, Month, Day}, {Hour, Minute, Second}}).

get_creation_time(TableName) ->
    case ets:lookup(creation_times, TableName) of
        [{TableName, CreationTimeSeconds}] ->
            CreationTimeSeconds;
        [] ->
            undefined
    end.
