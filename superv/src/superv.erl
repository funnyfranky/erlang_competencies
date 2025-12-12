-module(superv).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Workers =
        [worker_spec(Id) || Id <- [1,2,3]],

    BabySupervisors =
        [subsup_spec(Id) || Id <- [1,2]],

    Childs = Workers ++ BabySupervisors,

    {ok, {{one_for_one, 3, 10}, Childs}}.

worker_spec(Id) ->
    {
        {pool_worker, Id},
        {pool_worker, start_link, [Id]},
        permanent,
        5000,
        worker,
        [pool_worker]
    }.

subsup_spec(Id) ->
    {
        {subsuper, Id},
        {subsuper, start_link, [Id]},
        permanent,
        5000,
        supervisor,
        [subsuper]
    }.
