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
add({E, _, _}, Value) when Value == E ->
    {value_already_exists, Value}.

% Example
% 1> T1 = binary_search_trees:add(nil, 10).
% {10,nil,nil}
%
% 2> T2 = binary_search_trees:add(T1, 5).
% {10,{5,nil,nil},nil}
%
% 3> T3 = binary_search_trees:add(T2, 15).
% {10,{5,nil,nil},{15,nil,nil}}

remove(R, Value) ->
    {todo, R, Value}.

contains(nil, _Value) ->
    false;
contains({Value, _, _}, Value) ->
    true;
contains({V, Left, _Right}, Value) when Value < V ->
    contains(Left, Value);
contains({V, _Left, Right}, Value) when Value > V ->
    contains(Right, Value).

% Example
% binary_search_trees:contains(T3, 15).
% true.
% binary_search_trees:contains(T3, 5).
% true.
% binary_search_trees:contains(T3, 10).
% true.
% binary_search_trees:contains(T3, 3).
% false.
% binary_search_trees:contains(T3, 8).
% false.
% binary_search_trees:contains(T3, 200).
% false.


min(Tree) ->
    {todo, Tree}.

max(Tree) ->
    {todo, Tree}.

toList(Tree) ->
    {todo, Tree}.

fromList(Tree) ->
    {todo, Tree}.

height(Tree) ->
    {todo, Tree}.

isBalanced(Tree) ->
    {todo, Tree}.