-module(randomAlists).

-export([
    empty/1,
    cons/2,
    head/1,
    tail/1,
    lookup/2,
    update/3,

    find_tree/3,
    build_traversal_list/2,
    find_using_traversal_list/2,
    update/4,
    replace/3,
    merge/2,

    size/1,
    is_leaf/1
]).

empty([]) ->
    true;
empty(_) ->
    false.

head([H | _T]) ->
    H,
    randomAlists:lookup([H],0).
tail(RAL) ->
    [H | _T] = lists:reverse(RAL),
    {Index, _Right, _Left} = H,
    randomAlists:lookup([H],Index - 1).

size({_, nil, nil}) ->
    1;
size({Count, _Left, _Right}) ->
    Count.

is_leaf({_, nil, nil}) ->
    true;
is_leaf(_) ->
    false.

lookup(RAL, Index) when Index >= 0 ->
    {Offset, Tree} = find_tree(RAL, Index, 0),
    LocalIndex = Index - Offset,
    TreeSize = randomAlists:size(Tree),
    Path = build_traversal_list(LocalIndex, TreeSize),
    find_using_traversal_list(Tree, Path);
lookup(_RAL, Index) ->
    erlang:error({invalid_index, Index}).

find_tree([], _Index, _Acc) ->
    erlang:error(index_out_of_bounds);
find_tree([Tree | Rest], Index, Acc) ->
    TreeSize = randomAlists:size(Tree),
    case Index < Acc + TreeSize of
        true ->
            {Acc, Tree};
        false ->
            find_tree(Rest, Index, Acc + TreeSize)
    end.

build_traversal_list(Index, LocalSize) when
        Index >= 0, LocalSize > 0 ->
    do_build_traversal_list(Index, LocalSize).

do_build_traversal_list(_Index, 1) ->
    [];
do_build_traversal_list(Index, Size) ->
    Half = Size div 2,
    case Index < Half of
        true  -> [0 | do_build_traversal_list(Index, Half)];
        false -> [1 | do_build_traversal_list(Index - Half, Half)]
    end.

find_using_traversal_list({Value, nil, nil}, []) ->
    Value;

find_using_traversal_list({_Count, Left, _Right}, [0 | Rest]) ->
    find_using_traversal_list(Left, Rest);
find_using_traversal_list({_Count, _Left, Right}, [1 | Rest]) ->
    find_using_traversal_list(Right, Rest);

find_using_traversal_list(_Tree, _Path) ->
    erlang:error(invalid_path).

update(RAL, Index, NewValue) ->
    update(RAL, Index, NewValue, 0).

update([], _Index, _NewValue, _Acc) ->
    erlang:error(index_out_of_bounds);
update([Tree | Rest], Index, NewValue, Acc) ->
    TreeSize = randomAlists:size(Tree),
    case Index < Acc + TreeSize of
        true ->
            LocalIndex = Index - Acc,
            Path = build_traversal_list(LocalIndex, TreeSize),
            NewTree = replace(Tree, Path, NewValue),
            [NewTree | Rest];
        false ->
            [Tree | update(Rest, Index, NewValue, Acc + TreeSize)]
    end.

replace({_OldValue, nil, nil}, [], NewValue) ->
    {NewValue, nil, nil};

replace({Count, Left, Right}, [0 | Rest], NewValue) ->
    NewLeft = replace(Left, Rest, NewValue),
    {Count, NewLeft, Right};
replace({Count, Left, Right}, [1 | Rest], NewValue) ->
    NewRight = replace(Right, Rest, NewValue),
    {Count, Left, NewRight};

replace(_Tree, _Path, _NewValue) ->
    erlang:error(invalid_path).

cons(Value, RAL) ->
    Leaf = {Value, nil, nil},
    merge(Leaf, RAL).

merge(Tree, []) ->
    [Tree];
merge(Tree, [Head | Rest]) ->
    Size1 = randomAlists:size(Tree),
    Size2 = randomAlists:size(Head),
    case Size1 =:= Size2 of
        true ->
            NewTree = {Size1 + Size2, Tree, Head},
            merge(NewTree, Rest);
        false ->
            if
                Size1 < Size2 ->
                    [Tree, Head | Rest];
                true ->
                    [Head | merge(Tree, Rest)]
            end
    end.
