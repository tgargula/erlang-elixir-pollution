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

add_station_test() ->
  ?assertEqual(ok,pollution_server:add_station("stacja", {1,1})).

add_station2_test() ->
  ?assertEqual(ok,pollution_server:add_station("stacja2", {1,2})),
  ?assertMatch({error, _},pollution_server:add_station("stacja", {1,1})).

stop_server_test() ->
  pollution_server:stop(),
  timer:sleep(50),
  ?assert(not lists:member(server, registered())).
