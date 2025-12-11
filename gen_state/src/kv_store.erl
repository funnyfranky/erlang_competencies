-module(kv_store).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0, put/2, get/1, delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%%==============================
%%% External API
%%%==============================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

%%%==============================
%%% gen_server Callbacks
%%%==============================

init([]) ->
    %% Initial state: empty map
    {ok, #{}}.

handle_call({put, Key, Value}, _From, State) ->
    NewState = State#{Key => Value},
    {reply, ok, NewState};

handle_call({get, Key}, _From, State) ->
    Reply = maps:get(Key, State, undefined),
    {reply, Reply, State};

handle_call({delete, Key}, _From, State) ->
    NewState = maps:remove(Key, State),
    {reply, ok, NewState};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, error, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    io:format("Shutting down kv_store...~n"),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
