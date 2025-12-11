-module(genserver).

-behaviour(gen_server).

-export([
    start/0,
    init/1,
    enqueue/1,
    dequeue/0,
    clear_queue/0,
    handle_call/3,
    handle_cast/2
]).

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

enqueue(Message) ->
    gen_server:cast(?MODULE, {enqueue, Message}).

dequeue() ->
    gen_server:call(?MODULE, dequeue).

clear_queue() ->
    gen_server:cast(?MODULE, clear_queue).


init([]) ->
    {ok, {[], []}}.

handle_call(dequeue, _From, {[], []}) ->
    {reply, empty, {[], []}};

handle_call(dequeue, _From, {[], Rear}) ->
    [H|T] = lists:reverse(Rear),
    {reply, H, {T, []}};

handle_call(dequeue, _From, {[H|T], Rear}) ->
    {reply, H, {T, Rear}}.

handle_cast({enqueue, Msg}, {Front, Rear}) ->
    {noreply, {Front, [Msg | Rear]}};

handle_cast(clear_queue, _State) ->
    {noreply, {[], []}}.

