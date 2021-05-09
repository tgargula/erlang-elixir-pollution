%%%-------------------------------------------------------------------
%%% @author tomek
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 25. Apr 2021 16:44
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("tomek").

-include_lib("eunit/include/eunit.hrl").

start_server_test() ->
  pollution_server:start(),
  ?assert(lists:member(server, registered())).

add_station1_test() ->
  ?assertEqual(ok, pollution_server:add_station("Station", {1,1})).

add_station2_test() ->
  ?assertEqual(ok, pollution_server:add_station("Station 2", {1,2})),
  %% Station exists error
  ?assertMatch({error, _},pollution_server:add_station("Station", {1,1})).

add_value_test() -> 
  %% Add values that will be useful later and test them
  ?assertEqual(ok, pollution_server:add_value("Station", {{2021,4,25}, {17,27,52}}, "PM10", 5)),
  ?assertEqual(ok, pollution_server:add_value("Station 2", {{2021,4,25}, {17,27,52}}, "PM10", 10)),
  ?assertEqual(ok, pollution_server:add_value("Station", {{2021,4,26}, {17,27,52}}, "PM10", 15)),
  ?assertEqual(ok, pollution_server:add_value("Station", {{2021,4,26}, {17,27,52}}, "PM10", 25)),
  ?assertEqual(ok, pollution_server:add_value("Station", {{2021,4,25}, {19,00,52}}, "PM10", 13.5)),
  ?assertEqual(ok, pollution_server:add_value("Station", {{2021,4,25}, {17,00,52}}, "PM2.5", 1)),
  ?assertEqual(ok, pollution_server:add_value("Station 2", {{2021,4,25}, {17,27,52}}, "PM2.5", 2.5)),
  %% Non-existing station error
  ?assertMatch({error, _}, pollution_server:add_value("Non-existing station", calendar:local_time(), "PM10", 6)),
  %% The same measurement error
  ?assertMatch({error, _}, pollution_server:add_value("Station", {{2021,4,25}, {17,27,52}}, "PM10", 5)).
  
remove_value_test() -> 
  pollution_server:add_value("Station", {{2021,4,25}, {17,00,00}}, "PM10", 6),
  ?assertEqual(ok, pollution_server:remove_value("Station", {{2021,4,25}, {17,00,00}}, "PM10")).

get_one_value_test() -> 
  ?assertEqual({measurement,{{2021,4,25},{19,0,52}},"PM10",13.5}, pollution_server:get_one_value("Station", {{2021,4,25}, {19,00,52}}, "PM10")),
  ?assertEqual({measurement,{{2021,4,25},{17,27,52}},"PM2.5",2.5}, pollution_server:get_one_value("Station 2", {{2021,4,25}, {17,27,52}}, "PM2.5")).

get_station_mean_test() -> 
  ?assertEqual(14.625, pollution_server:get_station_mean("Station", "PM10")),
  ?assertEqual(2.5, pollution_server:get_station_mean("Station 2", "PM2.5")).

get_daily_mean_test() -> 
  ?assertEqual(9.5, pollution_server:get_daily_mean("PM10", {2021,4,25})),
  ?assertEqual(1.75, pollution_server:get_daily_mean("PM2.5", {2021,4,25})).

get_maximum_variation_station_test() ->
  ?assertEqual({"Station",{1,1},50.421875}, pollution_server:get_maximum_variation_station("PM10")),
  ?assertEqual({"Station",{1,1},0.0}, pollution_server:get_maximum_variation_station("PM2.5")).

get_station_variation_test() ->
  ?assertEqual(50.421875, pollution_server:get_station_variation("Station", "PM10")),
  ?assertEqual(0.0, pollution_server:get_station_variation("Station 2", "PM2.5")).

get_stats_test() -> 
  ?assertEqual(#{numberOfStations => 2,types => ["PM10","PM2.5"]}, pollution_server:get_stats()).

stop_server_test() ->
  pollution_server:stop(),
  timer:sleep(50),
  ?assert(not lists:member(server, registered())).
