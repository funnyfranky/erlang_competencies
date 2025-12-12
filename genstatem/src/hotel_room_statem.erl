-module(hotel_room_statem).
-behaviour(gen_statem).

-export([start/0, check_in/0, check_out/0, start_cleaning/0, finish_cleaning/0]).
-export([init/1, callback_mode/0]).
-export([vacant/3, occupied/3, cleaning/3]).

start() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

check_in() ->
    gen_statem:cast(?MODULE, check_in).

check_out() ->
    gen_statem:cast(?MODULE, check_out).

start_cleaning() ->
    gen_statem:cast(?MODULE, start_cleaning).

finish_cleaning() ->
    gen_statem:cast(?MODULE, finish_cleaning).



init([]) ->
    {ok, vacant, #{}}.

callback_mode() ->
    state_functions.


vacant(cast, check_in, Data) ->
    {next_state, occupied, Data};

vacant(cast, start_cleaning, Data) ->
    {next_state, cleaning, Data};

vacant(cast, check_out, Data) ->
    {keep_state, Data};

vacant(_, _, Data) ->
    {keep_state, Data}.


occupied(cast, check_out, Data) ->
    {next_state, vacant, Data};

occupied(cast, start_cleaning, Data) ->
    {keep_state, Data};

occupied(cast, check_in, Data) ->
    {keep_state, Data};

occupied(_, _, Data) ->
    {keep_state, Data}.


cleaning(cast, finish_cleaning, Data) ->
    {next_state, vacant, Data};

cleaning(cast, check_in, Data) ->
    {keep_state, Data};

cleaning(cast, check_out, Data) ->
    {keep_state, Data};

cleaning(_, _, Data) ->
    {keep_state, Data}.
