-module(pollution_server).
-export([start/0, stop/0, init/0, add_station/2, add_value/4, remove_value/3, get_one_value/3, get_station_mean/2,
  get_daily_mean/2, get_maximum_variation_station/1, get_station_variation/2, get_stats/0]).

start() ->
  register(server, spawn(fun() -> init() end)).

stop() ->
  server ! stop.

process(Alias, Args) ->
  server ! {Alias, Args},
  receive
    {error, Msg} -> {error, Msg};
    _ -> ok
  end.

add_station(Station, Coordinates) ->
  process(add_station, {Station, Coordinates, self()}).

add_value(Id, Time, Type, Value) ->
  process(add_value, {Id, Time, Type, Value, self()}).

remove_value(Id, Time, Type) ->
  process(remove_value, {Id, Time, Type, self()}).

get_one_value(Id, Time, Type) ->
  process(get_one_value, {Id, Time, Type, self()}).

get_station_mean(Id, Type) ->
  process(get_station_mean, {Id, Type, self()}).

get_daily_mean(Type, QueryDate) ->
  process(get_daily_mean, {Type, QueryDate, self()}).

get_maximum_variation_station(Type) ->
  process(get_maximum_variation_station, {Type, self()}).

get_station_variation(Id, Type) ->
  process(get_station_variation, {Id, Type, self()}).

get_stats() ->
  process(get_stats, {self()}).

init() ->
  Monitor = pollution:create_monitor(),
  run(Monitor).

handle_error(Monitor, NewMonitor, Pid) ->
  Pid ! NewMonitor,
  case NewMonitor of
    {error, _} -> run(Monitor);
    _ -> run(NewMonitor)
  end.

run(Monitor) ->
  receive
    stop -> stop;
    {add_station, {Station, Coordinates, Pid}} ->
      NewMonitor = pollution:add_station(Station, Coordinates, Monitor),
      handle_error(Monitor, NewMonitor, Pid);
    {add_value, {Id, Time, Type, Value, Pid}} ->
      NewMonitor = pollution:add_value(Id, Time, Type, Value, Monitor),
      handle_error(Monitor, NewMonitor, Pid);
    {remove_value, {Id, Time, Type, Pid}} ->
      NewMonitor = pollution:remove_value(Id, Time, Type, Monitor),
      Pid ! NewMonitor,
      run(NewMonitor);
    {get_one_value, {Id, Time, Type, Pid}} ->
      Pid ! pollution:get_one_value(Id, Time, Type, Monitor),
      run(Monitor);
    {get_station_mean, {Id, Type, Pid}} ->
      Pid ! pollution:get_station_mean(Id, Type, Monitor),
      run(Monitor);
    {get_daily_mean, {Type, QueryDate, Pid}} ->
      Pid ! pollution:get_daily_mean(Type, QueryDate, Monitor),
      run(Monitor);
    {get_maximum_variation_station, {Type, Pid}} ->
      Pid ! pollution:get_maximum_variation_station(Type, Monitor),
      run(Monitor);
    {get_station_variation, {Id, Type, Pid}} ->
      Pid ! pollution:get_station_variation(Id, Type, Monitor),
      run(Monitor);
    {get_stats, {Pid}} ->
      Pid ! pollution:get_stats(Monitor),
      run(Monitor)
  end.
