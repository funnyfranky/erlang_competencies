-module(binary_search_trees).

% In your editor, implement the following Binary Search Tree (BST) functions using the structure `{value, next_left, next_right}`. Each node consists of a value and references to the left and right subtrees:

% 1. `empty(bst)`:

%    * Input: A BST.
%    * Output: `true` if the BST is empty, `false` otherwise.

% 2. `add(bst, element)`:

%    * Input: A BST and an element.
%    * Output: A new BST with the element added.

% 3. `contains(bst, element)`:

%    * Input: A BST and a value.
%    * Output: `true` if the value is found, `false` otherwise.

% 4. `remove(bst, element)`:

%    * Input: A BST and an element.
%    * Output: A new BST with the element removed.

% 5. `min(bst)`:

%    * Input: A BST.
%    * Output: The smallest element or `nil` if the tree is empty.

% 6. `max(bst)`:

%    * Input: A BST.
%    * Output: The largest element or `nil` if the tree is empty.

% 7. `toList(bst)`:

%    * Input: A BST.
%    * Output: A list of elements in sorted order.

% 8. `fromList(list)`:

%    * Input: A list of elements.
%    * Output: A BST.

% 9. `height(bst)`:

%    * Input: A BST.
%    * Output: The height as an integer.

% 10. `isBalanced(bst)`:

% * Input: A BST.
% * Output: `true` if the tree is balanced, `false` otherwise.

% You must:

% * Use functional programming principles.
% * Write tests to validate the correctness of each function.
% * Provide tests for various scenarios, including:

%   * Adding to an empty BST.
%   * Removing the root, leaf nodes, and nodes with one or two children.
%   * Searching for elements at different depths.
%   * Converting between lists and BSTs.

% Challenge Extension (Optional):

% * Implement `map(bst, function)` that applies a function to all elements in the BST, returning a new tree.
% * Implement `merge(bst1, bst2)` that combines two BSTs into one.
% * Implement `countNodes(bst)` that returns the total number of nodes in the BST.

-export([empty/1, add/2, remove/2, contains/2, min/1,
max/1, toList/1, fromList/1, height/1, isBalanced/1]).

empty(nil) ->
    true;
empty({nil,nil,nil}) ->
    true;
empty(_) ->
    false.


add(nil, Value) ->
    {Value, nil, nil};
add({E, Left, Right}, Value) when Value < E ->
    {E, add(Left, Value), Right};
add({E, Left, Right}, Value) when Value > E ->
    {E, Left, add(Right, Value)};
add({E, Left, Right}, Value) when Value == E ->
    {E, Left, Right}.

contains(nil, _Value) ->
    false;
contains({Value, _, _}, Value) ->
    true;
contains({V, Left, _Right}, Value) when Value < V ->
    contains(Left, Value);
contains({V, _Left, Right}, Value) when Value > V ->
    contains(Right, Value).

remove(nil, _Value) ->
    nil;
remove({Value, nil, nil}, Value) ->
    nil;
remove({Value, Left, nil}, Value) ->
    Left;
remove({Value, nil, Right}, Value) ->
    Right;
remove({Value, Left, Right}, Value) ->
    Min = min(Right),
    NewRight = remove(Right,Min),
    {Min, Left, NewRight};
remove({V, Left, Right}, Value) when Value < V ->
    {V, remove(Left, Value), Right};
remove({V, Left, Right}, Value) when Value > V ->
    {V, Left, remove(Right, Value)}.

min({V, nil, _Right}) ->
    V;
min({_V, Left, _Right}) ->
    min(Left).

max({V, _Left, nil}) ->
    V;
max({_V, _Left, Right}) ->
    max(Right).

toList(nil) ->
    [];
toList({Value, Left, Right}) ->
    toList(Left) ++ [Value] ++ toList(Right).

fromList([]) ->
    nil;
fromList([H|T]) ->
    fromList(T, {H, nil, nil}).

fromList([], Tree) ->
    Tree;
fromList([H|T], Tree) ->
    fromList(T, add(Tree, H)).


height(nil) ->
    0;
height({_, Left, Right}) ->
    1 + max(height(Left), height(Right)).

isBalanced(Tree) ->
    {Balanced, _Height} = balanced_height(Tree),
    Balanced.

balanced_height(nil) ->
    {true, 0};
balanced_height({_, Left, Right}) ->
    {LeftBalanced, LeftH} = balanced_height(Left),
    {RightBalanced, RightH} = balanced_height(Right),
    Height = 1 + max(LeftH, RightH),
    Diff = abs(LeftH - RightH),
    Balanced = LeftBalanced andalso RightBalanced andalso (Diff =< 1),
    {Balanced, Height}.
