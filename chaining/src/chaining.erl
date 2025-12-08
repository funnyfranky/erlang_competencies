-module(chaining).
-export([run/1]).

chain(Data, []) ->
    Data;
chain(Data, [F | Rest]) ->
    chain(F(Data), Rest).


nums(List) when is_list(List) ->
    [X || X <- List, is_number(X)].

keep_positives(List) ->
    [N || N <- List, N > 0].
    
average([]) ->
    empty_list;
average(Ns) ->
    lists:sum(Ns) / length(Ns).    

run(List) ->
    chain(List, [
        fun nums/1,
        fun keep_positives/1,
        fun average/1
    ]).
