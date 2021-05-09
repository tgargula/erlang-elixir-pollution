%% This module was necessary before creating rebar3 app
%% At the moment pollution_sup.erl takes its responsibility

-module(pollution_supervisor).
-behaviour(supervisor).
-define(SUPERVISOR, pollution_supervisor).
-export([start_link/0, init/1]).

start_link() ->
  supervisor:start_link({local, ?SUPERVISOR}, ?MODULE, []),
  unlink(whereis(?SUPERVISOR)).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 1, period => 5},
  ChildSpecs = [#{id => pollution_gen_server,
    start => {pollution_gen_server, start_link, []},
    restart => permanent,
    shutdown => brutal_kill,
    type => worker,
    modules => [pollution_gen_server]}],
  {ok, {SupFlags, ChildSpecs}}.