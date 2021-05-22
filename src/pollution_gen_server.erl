-module(pollution_gen_server).
-author("tgargula").
-behaviour(gen_server).
-export([start_link/0, start/0, stop/0, crash/0, get/0, add_station/2, add_value/4, remove_value/3, get_one_value/3]).
-export([get_station_mean/2, get_daily_mean/2, get_maximum_variation_station/1, get_station_variation/2, get_stats/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-include("../include/pollution.hrl").

%% START %%
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  Monitor = pollution:create_monitor(),
  case Monitor of
    {error, Msg} -> {error, Msg};
    _ -> {ok, Monitor}
  end.

%% CLIENT -> SERVER %%
start() -> gen_server:call(?MODULE, start).
stop() -> gen_server:call(?MODULE, terminate).
crash() -> gen_server:cast(?MODULE, crash).
get() -> gen_server:call(?MODULE, get).
add_station(Station, Coordinates) -> gen_server:call(?MODULE, {add_station, Station, Coordinates}).
add_value(Id, Time, Type, Value) -> gen_server:call(?MODULE, {add_value, Id, Time, Type, Value}).
remove_value(Id, Time, Type) -> gen_server:call(?MODULE, {remove_value, Id, Time, Type}).
get_one_value(Id, Time, Type) -> gen_server:call(?MODULE, {get_one_value, Id, Time, Type}).
get_station_mean(Id, Type) -> gen_server:call(?MODULE, {get_station_mean, Id, Type}).
get_daily_mean(Type, QueryDate) -> gen_server:call(?MODULE, {get_daily_mean, Type, QueryDate}).
get_maximum_variation_station(Type) -> gen_server:call(?MODULE, {get_maximum_variation_station, Type}).
get_station_variation(Id, Type) -> gen_server:call(?MODULE, {get_station_variation, Id, Type}).
get_stats() -> gen_server:call(?MODULE, {get_stats}).


%% HANDLERS %%
handle_cast(crash, State) -> no:exist(), {noreply, State}.

handle_call(get, _From, State) -> {reply, State, State};

handle_call(start, _From, State) -> {noreply, State};

handle_call(stop, _From, State) -> {stop, normal, ok, State};

handle_call({add_station, Station, Coordinates}, _From, State) ->
  NewState = pollution:add_station(Station, Coordinates, State),
  case NewState of
    {error, Msg} -> {reply, {error, Msg}, State};
    _ ->
      pollution_database_gen_server:add_station(Station, Coordinates),
      {reply, ok, NewState}
  end;

handle_call({add_value, Id, Time, Type, Value}, _From, State) ->
  NewState = pollution:add_value(Id, Time, Type, Value, State),
  case NewState of
    {error, Msg} -> {reply, {error, Msg}, State};
    _ ->
      pollution_database_gen_server:add_value(pollution:get_key(Id, State), Time, Type, Value),
      {reply, ok, NewState}
  end;

handle_call({remove_value, Id, Time, Type}, _From, State) ->
  {reply, ok, pollution:remove_value(Id, Time, Type, State)};

handle_call({get_one_value, Id, Time, Type}, _From, State) ->
  {reply, pollution:get_one_value(Id, Time, Type, State), State};

handle_call({get_station_mean, Id, Type}, _From, State) ->
  {reply, pollution:get_station_mean(Id, Type, State), State};

handle_call({get_daily_mean, Type, QueryDate}, _From, State) ->
  {reply, pollution:get_daily_mean(Type, QueryDate, State), State};

handle_call({get_maximum_variation_station, Type}, _From, State) ->
  {reply, pollution:get_maximum_variation_station(Type, State), State};

handle_call({get_station_variation, Id, Type}, _From, State) ->
  {reply, pollution:get_station_variation(Id, Type, State), State};

handle_call({get_stats}, _From, State) ->
  {reply, pollution:get_stats(State), State}.

terminate(normal, _State) -> ok;
terminate(Reply, _State) -> Reply.
