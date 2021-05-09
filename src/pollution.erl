-module(pollution).
-author("tgargula").
-export([create_monitor/0, add_station/3, add_value/5, remove_value/4, get_one_value/4, get_station_mean/3, get_daily_mean/3,
  get_maximum_variation_station/2, get_station_variation/3, get_stats/1, get_key/2]).

%% A following header includes amongst others a measurement record definition
-include("../include/pollution.hrl").

%% Monitor data structure:
%% It is a map that stores two types of key-value pairs. One of them is K: Station, V: Coordinates
%% and the second is K: Coordinates, V: list of measurements
%% Measurements are represented by a record (that is a tuple with syntactic sugar)

create_monitor() -> #{}.

add_station(Station, Coordinates, Monitor) ->
%% Do not add the same station
  case maps:is_key(Station, Monitor) orelse maps:is_key(Coordinates, Monitor) of
    false -> Monitor#{Station => Coordinates, Coordinates => []};
    _ -> {error, "Station not unique!"}
  end.

add_measurement(Coordinates, Measurement, Monitor) ->
%% Do not add the same measurement
  case lists:member(Measurement, maps:get(Coordinates, Monitor)) of
    false -> Monitor#{Coordinates => [Measurement | maps:get(Coordinates, Monitor)]};
    _ -> {error, "Cannot add the same measurement!"}
  end.

add_value(Id, Time, Type, Value, Monitor) ->
%% Add only if the station exists
  case maps:is_key(Id, Monitor) of
    true ->
      Coordinates = get_key(Id, Monitor),
      Measurement = #measurement{time = Time, type = Type, value = Value},
      add_measurement(Coordinates, Measurement, Monitor);
    _ -> {error, "Station does not exist!"}
  end.

remove_value(Id, Time, Type, Monitor) ->
  Coordinates = get_key(Id, Monitor),
  Fun = fun(X) -> X#measurement.time =/= Time orelse X#measurement.type =/= Type end,
  Monitor#{Coordinates => lists:filter(Fun, maps:get(Coordinates, Monitor))}.

get_one_value(Id, Time, Type, Monitor) ->
  Fun = fun(X) -> X#measurement.time == Time andalso X#measurement.type == Type end,
  [Result | _] = lists:filter(Fun, maps:get(get_key(Id, Monitor), Monitor)),
  Result.

get_station_mean(Id, Type, Monitor) ->
  Measurements = lists:filter(fun(X) -> X#measurement.type == Type end, maps:get(get_key(Id, Monitor), Monitor)),
  lists:foldl(fun(X, Y) -> X#measurement.value + Y end, 0, Measurements) / length(Measurements).

get_daily_mean(Type, QueryDate, Monitor) ->
  Pred1 = fun(K, _) -> case K of {_, _} -> true; _ -> false end end,
  Pred2 = fun(X) -> {Date, _} = X#measurement.time, Date == QueryDate andalso X#measurement.type == Type end,
  Measurements = lists:filter(Pred2, lists:flatten(maps:values(maps:filter(Pred1, Monitor)))),
  lists:foldl(fun(X, Y) -> X#measurement.value + Y end, 0, Measurements) / length(Measurements).

%% Returns a station with the highest variation of values of the given type
get_maximum_variation_station(Type, Monitor) ->
  Stations = maps:to_list(maps:filter(fun(Key, _) -> case Key of {_, _} -> false; _ -> true end end, Monitor)),
  StationsWithVariations = lists:map(
    fun({StationName, Coordinates}) -> {StationName, Coordinates, get_station_variation(Coordinates, Type, Monitor)} end,
    Stations
  ),
  Max = fun(Measurement, CurrentMax) ->
    {_, _, Value} = Measurement,
    {_, _, CurrentMaxValue} = CurrentMax,
    case Value > CurrentMaxValue of
      true -> Measurement;
      _ -> CurrentMax
    end end,
  lists:foldl(Max, {null, null, -1}, StationsWithVariations).

%% Returns the variation of values of the given Type of the station with the given Id
get_station_variation(Id, Type, Monitor) ->
  Measurements = lists:filter(fun(X) -> X#measurement.type == Type end, maps:get(get_key(Id, Monitor), Monitor)),
  Mean = get_station_mean(Id, Type, Monitor),
  case length(Measurements) > 0 of
    true ->
      lists:foldl(fun(X, Y) -> math:pow(X#measurement.value - Mean, 2) + Y end, 0, Measurements) / length(Measurements);
    _ -> 0
  end.

%% Returns number of stations and all distinct measurement types registered by Monitor
get_stats(Monitor) ->
  NumberOfStations = maps:size(Monitor) div 2,
  Types = [ Measurement#measurement.type || {_, Value} <- maps:to_list(Monitor), is_list(Value), Measurement <- Value],
  #{
    numberOfStations => NumberOfStations,
    types => lists:usort(Types)
  }.

get_key(Id, Monitor) ->
  case Id of
    {_, _} -> Id;
    _ -> maps:get(Id, Monitor)
  end.
