-module(pollution_test).
-author("tgargula").
-include_lib("eunit/include/eunit.hrl").

create_monitor_test() ->
    M = pollution:create_monitor(),
    ?assertEqual(#{}, M).

add_station_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    ?assertEqual(#{"Station" => {15.0000, 16.5000}, {15.0000, 16.5000} => []}, M2).

add_station_duplicate_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_station("Station", {15.0000, 16.5000}, M2),
    M4 = pollution:add_station("Station 2", {13.0545, 21.3705}, M2),
    ?assertEqual(#{"Station" => {15.0000, 16.5000}, {15.0000, 16.5000} => []}, M2),
    ?assertMatch({error, _}, M3),
    ?assertEqual(
        #{
            "Station" => {15.0000, 16.5000}, 
            {15.0000, 16.5000} => [],
            "Station 2" => {13.0545, 21.3705},
            {13.0545, 21.3705} => []
        }, M4).

add_value_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{14,00,00}}, "PM10", 52, M3),
    M5 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M4),
    ?assertEqual(
        #{
            "Station" => {15.0000, 16.5000}, 
            {15.0000, 16.5000} => [{measurement, {{2021,4,25},{13,51,50}}, "PM10", 50}]
        }, M3),
    ?assertEqual(
        #{
            "Station" => {15.0000, 16.5000}, 
            {15.0000, 16.5000} => [
                {measurement, {{2021,4,25},{14,00,00}}, "PM10", 52},
                {measurement, {{2021,4,25},{13,51,50}}, "PM10", 50}
            ]
        }, M4),
    ?assertMatch({error, _}, M5).

remove_value_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:remove_value("Station", {{2021,4,25},{13,51,50}}, "PM10", M3),
    ?assertEqual(#{"Station" => {15.0000, 16.5000}, {15.0000, 16.5000} => []}, M4).

get_one_value_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    Measurement = pollution:get_one_value("Station", {{2021,4,25},{13,51,50}}, "PM10", M3),
    ?assertEqual({measurement, {{2021,4,25},{13,51,50}}, "PM10", 50}, Measurement).

get_station_mean_test() ->
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{14,00,00}}, "PM10", 52, M3),
    M5 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{15,00,00}}, "PM10", 36, M4),
    Mean = pollution:get_station_mean("Station", "PM10", M5),
    ?assertEqual(46.0, Mean).

get_daily_mean_test() -> 
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{14,00,00}}, "PM2.5", 52, M3),
    M5 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{15,00,00}}, "PM10", 36, M4),
    M6 = pollution:add_value("Station", {{2021,4,24},{13,51,50}}, "PM10", 100, M5),
    M7 = pollution:add_station("Station 2", {14.0, 12.0}, M6),
    M8 = pollution:add_value("Station 2", {{2021,4,25},{14,0,0}}, "PM10", 0, M7),
    Mean = pollution:get_daily_mean("PM10", {2021,4,25}, M8),
    ?assertEqual(28.666666666666668, Mean).

get_maximum_variation_station_test() -> 
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{14,00,00}}, "PM2.5", 52, M3),
    M5 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{15,00,00}}, "PM10", 36, M4),
    M6 = pollution:add_value("Station", {{2021,4,24},{13,51,50}}, "PM10", 100, M5),
    M7 = pollution:add_station("Station 2", {14.0, 12.0}, M6),
    M8 = pollution:add_value("Station 2", {{2021,4,25},{14,0,0}}, "PM10", 0, M7),
    Station = pollution:get_maximum_variation_station("PM10", M8),
    ?assertEqual({"Station",{15.0,16.5},754.6666666666666}, Station).

get_station_variation_test() -> ok.

get_stats_test() -> 
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    M3 = pollution:add_value("Station", {{2021,4,25},{13,51,50}}, "PM10", 50, M2),
    M4 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{14,00,00}}, "PM2.5", 52, M3),
    M5 = pollution:add_value({15.0000, 16.5000}, {{2021,4,25},{15,00,00}}, "PM10", 36, M4),
    M6 = pollution:add_value("Station", {{2021,4,24},{13,51,50}}, "PM10", 100, M5),
    M7 = pollution:add_station("Station 2", {14.0, 12.0}, M6),
    M8 = pollution:add_value("Station 2", {{2021,4,25},{14,0,0}}, "PM10", 0, M7),
    Stats = pollution:get_stats(M8),
    ?assertEqual(#{numberOfStations => 2,types => ["PM10","PM2.5"]}, Stats).

get_key_test() -> 
    M1 = pollution:create_monitor(),
    M2 = pollution:add_station("Station", {15.0000, 16.5000}, M1),
    ?assertEqual({15.0000, 16.5000}, pollution:get_key("Station", M2)),
    ?assertEqual({15.0000, 16.5000}, pollution:get_key({15.0000, 16.5000}, M2)).