-module(pool_worker).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_info/2, terminate/2]).
-export([handle_call/3, handle_cast/2]).

start_link(Id) ->
    gen_server:start_link(?MODULE, Id, []).

init(Id) ->
    timer:send_interval(3000, do_work),
    io:format("Worker ~p started~n", [Id]),
    {ok, #{id => Id, jobs_done => 0}}.

handle_info(do_work, State) ->
    maybe_crash(),
    NewCount = maps:get(jobs_done, State) + 1,
    io:format("Worker ~p did job ~p~n",
              [maps:get(id, State), NewCount]),
    {noreply, State#{jobs_done => NewCount}};

handle_info(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    io:format("Worker ~p terminated: ~p~n",
              [maps:get(id, State), Reason]),
    ok.

maybe_crash() ->
    case rand:uniform(8) of
        1 -> exit(simulated_failure);
        _ -> ok
    end.
