-module(binary_tree).
-export([empty/0, new_node/1, left/1, right/1, value/1, leaf/1]).

% Format: 
    % Tree:
    % {Node value, Left subtree, Right Subtree}
% Possibilities:
% Num, tree, nil
% Num, nil, tree
% Num, tree, tree

% Num, leaf, leaf
% Num, tree, leaf
% Num, leaf, tree
% Num, nil, leaf
% Num, leaf, nil

%% empty tree
empty() ->
    nil.

% Leaf nodes - no subtrees
leaf(Value) when is_number(Value) ->
    Value.

% Creates a new tree node with Value, Left subtree, Right subtree
new_node({Value, Left, nil}) when is_number(Left) ->
    {Value, leaf(Left), empty()};

new_node({Value, nil, Right}) when is_number(Right) ->
    {Value, empty(), leaf(Right)};

new_node({Value, Left, nil}) when is_tuple(Left) ->
    {Value, new_node(Left), empty()};

new_node({Value, nil, Right}) when is_tuple(Right) ->
    {Value, empty(), new_node(Right)};

new_node({Value, Left, Right}) when is_number(Left),is_number(Right) ->
    {Value, leaf(Left), leaf(Right)};

new_node({Value, Left, Right}) when is_tuple(Left),is_number(Right) ->
    {Value, new_node(Left), leaf(Right)};

new_node({Value, Left, Right}) when is_number(Left),is_tuple(Right) ->
    {Value, leaf(Left), new_node(Right)};

new_node({Value, Left, Right}) when is_tuple(Left),is_tuple(Right) ->
    {Value, new_node(Left), new_node(Right)}.


%% Access the left subtree
left({_Value, Left, _Right}) ->
    Left.

%% Access the right subtree
right({_Value, _Left, Right}) ->
    Right.

%% Access the value at a node
value({Value, _Left, _Right}) ->
    Value.
