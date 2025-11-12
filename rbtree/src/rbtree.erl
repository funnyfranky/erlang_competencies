-module(rbtree).

-export([
    empty/1, add/2, make_black/1, contains/2, remove/2, min/1, max/1, toList/1, fromList/1
]).

% Functional Requirements:

% empty(rbt): Checks if the RBT is empty.
% Input: An RBT.
% Output: true if the tree is empty, false otherwise.
empty(nil) -> true;
empty(_)   -> false.

comparison(A, B) when A < B -> -1;
comparison(A, B) when A > B -> 1;
comparison(_, _) -> 0.

% add(rbt, element): Adds an element to the RBT.
% Input: An RBT and an element.
% Output: A new RBT with the element added.
add(Tree, E) ->
    {_, T} = rbt_add_helper(Tree, E, fun comparison/2),
    make_black(T).

rbt_add_helper(nil, E, _Cmp) ->
    {inserted, {red, E, nil, nil}};
rbt_add_helper({Color, V, L, R}, E, Cmp) ->
    case Cmp(E, V) of
        -1 ->
            {_, NL} = rbt_add_helper(L, E, Cmp),
            {ok, balance_l({Color, V, NL, R})};
        1 ->
            {_, NR} = rbt_add_helper(R, E, Cmp),
            {ok, balance_r({Color, V, L, NR})};
        0 ->
            {ok, {Color, V, L, R}}
    end.

balance_l({black, V, {red, LV, {red, LLV, LL, LR}, RL}, R}) ->
    {red, LV, {black, LLV, LL, LR}, {black, V, RL, R}};
balance_l({black, V, {red, LV, LL, {red, LRV, LRL, LRR}}, R}) ->
    {red, LRV, {black, LV, LL, LRL}, {black, V, LRR, R}};
balance_l(T) -> T.

balance_r({black, V, L, {red, RV, RL, {red, RRV, RRL, RRR}}}) ->
    {red, RV, {black, V, L, RL}, {black, RRV, RRL, RRR}};
balance_r({black, V, L, {red, RV, {red, RLV, RLL, RLR}, RR}}) ->
    {red, RLV, {black, V, L, RLL}, {black, RV, RLR, RR}};
balance_r(T) -> T.

make_black({_, V, L, R}) -> {black, V, L, R};
make_black(nil) -> nil.


% contains(rbt, element): Checks if a value exists in the RBT.
% Input: An RBT and a value.
% Output: true if the value is found, false otherwise.
contains(nil, _) -> false;
contains({_, V, L, R}, E) ->
    case comparison(E, V) of
        -1 -> contains(L, E);
        1  -> contains(R, E);
        0  -> true
    end.

% remove(rbt, element): Removes an element from the RBT.
% Input: An RBT and an element.
% Output: A new RBT with the element removed.
remove(nil, _) -> nil;
remove({C, V, L, R}, E) ->
    case comparison(E, V) of
        -1 -> {C, V, remove(L, E), R};
        1  -> {C, V, L, remove(R, E)};
        0 ->
            case {L, R} of
                {nil, nil} -> nil;
                {nil, _} -> R;
                {_, nil} -> L;
                _ ->
                    Min = min(R),
                    {C, Min, L, remove(R, Min)}
            end
    end.

% min(rbt): Finds the smallest element in the RBT.
% Input: An RBT.
% Output: The smallest element or nil if the tree is empty.
min(nil) -> nil;
min({_, V, nil, _}) -> V;
min({_, _, L, _}) -> min(L).


% max(rbt): Finds the largest element in the RBT.
% Input: An RBT.
% Output: The largest element or nil if the tree is empty.
max(nil) -> nil;
max({_, V, _, nil}) -> V;
max({_, _, _, R}) -> max(R).

% toList(rbt): Converts the RBT to a sorted list.
% Input: An RBT.
% Output: A list of elements in sorted order.
toList(nil) -> [];
toList({_, V, L, R}) ->
    toList(L) ++ [V] ++ toList(R).

% fromList(list): Constructs an RBT from a list of elements.
% Input: A list of elements.
% Output: An RBT.
fromList(List) ->
    lists:foldl(fun(E, Acc) -> add(Acc, E) end, nil, List).