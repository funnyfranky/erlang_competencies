-module(genstatem_example).
-behaviour(gen_statem).

-export([start_link/0]).
-export([init/1, callback_mode/0, handle_event/4]).

start_link() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, idle, #{}}.  % Initial state is 'idle' with empty data map

callback_mode() ->
    state_functions.

handle_event(cast, start, idle, Data) ->
    {next_state, processing, Data};

handle_event(_, _, State, Data) ->
    {next_state, State, Data}.
