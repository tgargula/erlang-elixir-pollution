-module(pollution_value_collector_gen_statem).
-behavior(gen_statem).
-author("tgargula").

-export([init/1, callback_mode/0]).
-export([start_link/0, stop/0, terminate/3]).
-export([set_station/1, add_value/3, store_data/0]).
-export([station/3, values/3]).

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_statem:stop(?MODULE).
terminate(_Reason, _StateName, _StateData) -> ok.

init(_Args) -> {ok, station, []}.
callback_mode() -> state_functions.

%% public API
set_station(Id) -> gen_statem:cast(?MODULE, {set_station, Id}).
add_value(Time, Type, Value) -> gen_statem:cast(?MODULE, {add_value, Time, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, {store_data}).

%% Handlers
%% There are only two states:
%% – station; to set first station
%% – values; to add values, set other station or flush data

station(_Event, {set_station, Id}, _State) ->
  {next_state, values, #{station => Id, measurements => []}}.

values(_Event, {add_value, Time, Type, Value}, State) ->
  Measurements = maps:get(measurements, State),
  NewState = State#{measurements := [{Time, Type, Value} | Measurements]},
  {keep_state, NewState};

%% Do not change station (thus stay in 'values' state)
values(_Event, {store_data}, State) ->
  flush_data(State),
  {keep_state, State#{measurements => []}};

%% setting station loads current data to the server and changes station (next state is 'values' as well)
values(_Event, {set_station, Id}, State) ->
  flush_data(State),
  {keep_state, #{station => Id, measurements => []}}.

flush_data(#{station := _Station, measurements := []}) -> ok;
flush_data(#{station := Station, measurements := [{Time, Type, Value} | Measurements]}) ->
  pollution_gen_server:add_value(Station, Time, Type, Value),
  flush_data(#{station => Station, measurements => Measurements}).
