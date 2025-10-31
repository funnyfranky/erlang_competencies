-module(binary_search_trees).

-export([add_node/2, contains/2]).

add_node(nil, Value) ->
    {Value, nil, nil};
add_node({E, Left, Right}, Value) when Value < E ->
    {E, add_node(Left, Value), Right};
add_node({E, Left, Right}, Value) when Value > E ->
    {E, Left, add_node(Right, Value)};
add_node({E, _, _}, Value) when Value == E ->
    {value_already_exists, Value}.

% Example
% 1> T1 = binary_search_trees:add_node(nil, 10).
% {10,nil,nil}
%
% 2> T2 = binary_search_trees:add_node(T1, 5).
% {10,{5,nil,nil},nil}
%
% 3> T3 = binary_search_trees:add_node(T2, 15).
% {10,{5,nil,nil},{15,nil,nil}}

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