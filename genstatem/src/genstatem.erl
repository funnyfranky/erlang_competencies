% In your editor, write and test a gen_statem for a hotel room 
% occupancy system. It should:

% Track states like Room Vacant, Room Occupied, and Cleaning.

% Transition based on check-in, check-out, and housekeeping actions.

% Enforce rules, such as no check-in during Cleaning.

-module(genstatem).
-behaviour(gen_statem).

-export([
    callback_mode/0,
    init/1,

    start/0,
    check_in/0,
    check_out/0,
    cleaning/0,
    finished_cleaning/0,

    vacant/3,
    occupied/3,
    cleaning/3,
    get_state/0
]).

start() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

check_in() ->
    gen_statem:cast(?MODULE, check_in).
check_out() ->
    gen_statem:cast(?MODULE, check_out).
cleaning() ->
    gen_statem:cast(?MODULE, cleaning).
finished_cleaning() ->
    gen_statem:cast(?MODULE, finished_cleaning).
get_state() ->
    gen_statem:call(?MODULE, get_state).


callback_mode() ->
    state_functions.
init([]) ->
    {ok, vacant, #{}}.


vacant(cast, check_in, Data) ->
    {next_state, occupied, Data};
vacant(cast, cleaning, Data) ->
    {next_state, cleaning, Data};
vacant({call, From}, get_state, Data) ->
    io:format("Vacant Status"),
    {keep_state, Data, [{reply, From, vacant}]};
vacant(_, _, Data) ->
    io:format("Staying as vacant"),
    {keep_state, Data}.

occupied(cast, check_out, Data) ->
    {next_state, vacant, Data};
occupied({call, From}, get_state, Data) ->
    io:format("Occupied Status"),
    {keep_state, Data, [{reply, From, occupied}]};
occupied(_, _, Data) ->
    io:format("Staying occuppied"),
    {keep_state, Data}.

cleaning(cast, finished_cleaning, Data) ->
    {next_state, vacant, Data};
cleaning({call, From}, get_state, Data) ->
    io:format("Cleaning Status~n"),
    {keep_state, Data, [{reply, From, cleaning}]};
cleaning(_, _, Data) ->
    io:format("Staying cleaning"),
    {keep_state, Data}.