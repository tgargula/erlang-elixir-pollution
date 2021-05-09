-module(pollution_value_collector_gen_statem).
-author("tgargula").
-behavior(gen_statem).
-export([init/1, callback_mode/0]).

start_link() -> gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).
stop() -> gen_statem:stop(?MODULE).
terminate(Reason, StateName, StateData) -> ok.

init(Args) -> {ok, set_station, []}.
callback_mode() -> state_functions.

%%
station() ->
  receive

  end

%% public API
set_station(Id) -> gen_statem:cast(?MODULE, {set_station, Id}).
add_value(Time, Type, Value) -> gen_statem:cast(?MODULE, {add_value, Time, Type, Value}).
store_data() -> gen_statem:cast(?MODULE, {store_data}).

%% Handlers
set
