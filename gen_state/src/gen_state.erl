-module(gen_state).

-export([]).

-export([start_link/0, stop/0, enqueue/1, dequeue/0, clear_queue/0]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%%==============================
%%% External API
%%%==============================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop).

enqueue(Message) ->
    gen_server:cast(?MODULE, {enqueue, Message}).

dequeue() ->
    gen_server:call(?MODULE, {dequeue}).

clear_queue() ->
    gen_server:cast(?MODULE, {clear_the_queue}).

%%%==============================
%%% gen_server Callbacks
%%%==============================

init([]) ->
    {ok, {[],[]}}.

handle_call({enqueue, _Data}, _From, State) ->
    %% Do something with the data and return a reply
    {todo, ok, State};

handle_call({dequeue}, _From, State) ->
    {todo, State};

handle_call({clear_queue}, _From, State) ->
    {todo, State};
    
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(_Request, _From, State) ->
    {reply, error, State}.



handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
