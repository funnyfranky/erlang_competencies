-module(subsuper).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(Id) ->
    supervisor:start_link(?MODULE, Id).

init(_Id) ->
    {ok, {{one_for_one, 1, 5}, []}}.
