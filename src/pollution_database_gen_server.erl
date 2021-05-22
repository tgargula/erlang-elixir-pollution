-module(pollution_database_gen_server).
-author("tgargula").
-behaviour(gen_server).
-export([handle_call/3, handle_cast/2]).
-export([start_link/0, init/1, terminate/2, stop/0]).
-export([add_station/2, add_value/4, get_pid/0, get_state/0, crash/0]).
-export([get_one_value/3, get_station_mean/2, get_daily_mean/2, get_maximum_variation_station/1,
  get_station_variation/2, get_stats/0, remove_value/3]).

-include("../include/pollution.hrl").

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
  {ok, Pid} = riakc_pb_socket:start("127.0.0.1", 8087),
  InitialState = pollution:create_monitor(),
  State = migrate(Pid, InitialState),
  {ok, {Pid, State}}.

migrate(Pid, State) ->
  {ok, StationKeys} = riakc_pb_socket:list_keys(Pid, ?STATION_BUCKET),
  Stations = lists:map(
    fun(Id) ->
      {ok, Fetched} = riakc_pb_socket:get(Pid, ?STATION_BUCKET, Id),
      binary_to_term(riakc_obj:get_value(Fetched))
    end,
    StationKeys
  ),
  {ok, MeasurementsKeys} = riakc_pb_socket:list_keys(Pid, ?MEASUREMENT_BUCKET),
  Measurements = lists:map(
    fun(Id) ->
      {ok, Fetched} = riakc_pb_socket:get(Pid, ?MEASUREMENT_BUCKET, Id),
      binary_to_term(riakc_obj:get_value(Fetched))
    end,
    MeasurementsKeys
  ),
  NewState = add_stations(Stations, State),
  add_measurements(Measurements, NewState).

add_stations([], State) -> State;
add_stations([#{station := StationName, coordinates := Coordinates} | Stations], State) ->
  add_stations(Stations, pollution:add_station(StationName, Coordinates, State)).

add_measurements([], State) -> State;
add_measurements([#{id := Id, time := Time, type := Type, value := Value} | Measurements], State) ->
  add_measurements(Measurements, pollution:add_value(Id, Time, Type, Value, State)).

%% CLIENT -> SERVER %%
get_pid() -> gen_server:call(?MODULE, get_pid).
get_state() -> gen_server:call(?MODULE, get_state).
crash() -> gen_server:cast(?MODULE, crash).
stop() -> gen_server:call(?MODULE, terminate).

add_station(Station, Coordinates) -> gen_server:call(?MODULE, {add_station, Station, Coordinates}).
add_value(Id, Time, Type, Value) -> gen_server:call(?MODULE, {add_value, Id, Time, Type, Value}).
remove_value(Id, Time, Type) -> gen_server:call(?MODULE, {remove_value, Id, Time, Type}). %% not implemented
get_one_value(Id, Time, Type) -> gen_server:call(?MODULE, {get_one_value, Id, Time, Type}).
get_station_mean(Id, Type) -> gen_server:call(?MODULE, {get_station_mean, Id, Type}).
get_daily_mean(Type, QueryDate) -> gen_server:call(?MODULE, {get_daily_mean, Type, QueryDate}).
get_maximum_variation_station(Type) -> gen_server:call(?MODULE, {get_maximum_variation_station, Type}).
get_station_variation(Id, Type) -> gen_server:call(?MODULE, {get_station_variation, Id, Type}).
get_stats() -> gen_server:call(?MODULE, {get_stats}).


%% HANDLERS %%
handle_cast(crash, State) -> no:exist(), {noreply, State}.

handle_call(stop, _From, State) -> {stop, normal, ok, State};

handle_call(get_pid, _From, {Pid, State}) -> {reply, Pid, {Pid, State}};

handle_call(get_state, _From, {Pid, State}) -> {reply, State, {Pid, State}};

handle_call({add_station, Station, Coordinates}, _From, {Pid, State}) ->
  NewState = pollution:add_station(Station, Coordinates, State),
  case NewState of
    {error, Msg} -> {reply, {error, Msg}, {Pid, State}};
    _ ->
      Key = term_to_binary(Coordinates),
      Obj = riakc_obj:new(?STATION_BUCKET, Key, #{station => Station, coordinates => Coordinates}),
      riakc_pb_socket:put(Pid, Obj),
      {reply, ok, {Pid, NewState}}
  end;

handle_call({add_value, Id, Time, Type, Value}, _From, {Pid, State}) ->
  NewState = pollution:add_value(Id, Time, Type, Value, State),
  case NewState of
    {error, Msg} -> {reply, {error, Msg}, {Pid, State}};
    _ ->
      Key = term_to_binary(Id),
      Obj = riakc_obj:new(?MEASUREMENT_BUCKET, Key, #{id => Id, time => Time, type => Type, value => Value}),
      riakc_pb_socket:put(Pid, Obj),
      {reply, ok, {Pid, NewState}}
  end;

handle_call({remove_value, Id, Time, Type}, _From, {Pid, State}) ->
  NewState = pollution:remove_value(Id, Time, Type, State),
  case NewState of
    {error, Msg} -> {reply, {error, Msg}, {Pid, State}};
    _ ->
      Key = term_to_binary(Id),
      riakc_pb_socket:delete(Pid, ?MEASUREMENT_BUCKET, Key),
      {reply, ok, {Pid, NewState}}
  end;

handle_call({get_one_value, Id, Time, Type}, _From, {Pid, State}) ->
  {reply, pollution:get_one_value(Id, Time, Type, State), {Pid, State}};

handle_call({get_station_mean, Id, Type}, _From, {Pid, State}) ->
  {reply, pollution:get_station_mean(Id, Type, State), {Pid, State}};

handle_call({get_daily_mean, Type, QueryDate}, _From, {Pid, State}) ->
  {reply, pollution:get_daily_mean(Type, QueryDate, State), {Pid, State}};

handle_call({get_maximum_variation_station, Type}, _From, {Pid, State}) ->
  {reply, pollution:get_maximum_variation_station(Type, State), {Pid, State}};

handle_call({get_station_variation, Id, Type}, _From, {Pid, State}) ->
  {reply, pollution:get_station_variation(Id, Type, State), {Pid, State}};

handle_call({get_stats}, _From, {Pid, State}) ->
  {reply, pollution:get_stats(State), {Pid, State}}.

terminate(normal, _State) -> ok;
terminate(Reply, _State) -> Reply.
