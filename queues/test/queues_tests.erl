-module(queues_tests).
-include_lib("eunit/include/eunit.hrl").

empty_test_() ->
    [
        ?_assert(queues:empty({[], []}) =:= true),
        ?_assert(queues:empty({[1], []}) =:= false),
        ?_assert(queues:empty({[], [1]}) =:= false)
    ].

enqueue_test_() ->
    Q0 = {[], []},
    Q1 = queues:enqueue(Q0, 1),
    Q2 = queues:enqueue(Q1, 2),
    Q3 = queues:enqueue(Q2, 3),
    [
        ?_assert(Q1 =:= {[], [1]}),
        ?_assert(Q2 =:= {[], [2,1]}),
        ?_assert(Q3 =:= {[], [3,2,1]})
    ].

dequeue_test_() ->
    Q0 = {[], []},
    {nil, EmptyQ} = queues:dequeue(Q0),
    Q1 = queues:enqueue(Q0, 10),
    Q2 = queues:enqueue(Q1, 20),
    {D1, Q3} = queues:dequeue(Q2),
    {D2, Q4} = queues:dequeue(Q3),
    [
        ?_assert(EmptyQ =:= {[], []}),
        ?_assert(D1 =:= 10),
        ?_assert(D2 =:= 20),
        ?_assert(Q4 =:= {[], []})
    ].

head_test_() ->
    Q0 = {[], []},
    Q1 = queues:enqueue(Q0, a),
    Q2 = queues:enqueue(Q1, b),
    [
        ?_assert(queues:head(Q0) =:= nil),
        ?_assert(queues:head(Q1) =:= a),
        ?_assert(queues:head(Q2) =:= a)
    ].

tail_test_() ->
    Q0 = {[], []},
    Q1 = queues:enqueue(Q0, a),
    Q2 = queues:enqueue(Q1, b),
    Q3 = queues:enqueue(Q2, c),
    [
        ?_assert(queues:tail(Q0) =:= nil),
        ?_assert(queues:tail(Q1) =:= a),
        ?_assert(queues:tail(Q2) =:= b),
        ?_assert(queues:tail(Q3) =:= c)
    ].

tolist_test_() ->
    Q0 = {[], []},
    Q1 = queues:enqueue(Q0, 1),
    Q2 = queues:enqueue(Q1, 2),
    Q3 = queues:enqueue(Q2, 3),
    [
        ?_assert(queues:toList(Q0) =:= []),
        ?_assert(queues:toList(Q3) =:= [1,2,3])
    ].

fromlist_test_() ->
    L = [a,b,c],
    Q = queues:fromList(L),
    [
        ?_assert(is_tuple(Q)),
        ?_assert(element(1, Q) =:= [L]),
        ?_assert(element(2, Q) =:= [])
    ].
