-module(leftMHeaps).

-export([
    new/0,
    empty/1,
    insert/2,
    findMin/1,
    deleteMin/1,
    merge/2,
    toList/1,
    fromList/1
]).

new() ->
    nil.

empty(nil) -> true;
empty(_)   -> false.

insert(Heap, Element) ->
    Singleton = {1, Element, nil, nil},
    merge(Singleton, Heap).

findMin(nil) ->
    erlang:error({empty_heap, findMin});
findMin({_, Value, _, _}) ->
    Value.

deleteMin(nil) ->
    erlang:error({empty_heap, deleteMin});
deleteMin({_, _Value, Left, Right}) ->
    merge(Left, Right).

merge(nil, H) ->
    H;
merge(H, nil) ->
    H;
merge(_H1 = {_, V1, L1, R1}, H2 = {_, V2, _L2, _R2}) when V1 =< V2 ->
    make_node(V1, L1, merge(R1, H2));
merge(H1, {_, V2, L2, R2}) ->
    make_node(V2, L2, merge(H1, R2)).

toList(Heap) ->
    toList(Heap, []).

toList(nil, Acc) ->
    lists:reverse(Acc);
toList(Heap, Acc) ->
    Min = findMin(Heap),
    toList(deleteMin(Heap), [Min | Acc]).

fromList(List) ->
    lists:foldl(
      fun(Element, HeapAcc) ->
              insert(HeapAcc, Element)
      end,
      nil,
      List
    ).

rank(nil) ->
    0;
rank({R, _V, _L, _R}) ->
    R.

make_node(Value, Left, Right) ->
    RLeft  = rank(Left),
    RRight = rank(Right),
    case RLeft >= RRight of
        true  ->
            {RRight + 1, Value, Left, Right};
        false ->
            {RLeft + 1, Value, Right, Left}
    end.