-module(functor).

-export([map_tree/2]).

%% Applies a function to every value in the tree without changing structure
map_tree(_Fun, nil) ->
    nil;

map_tree(Fun, {Value, Left, Right}) ->
    {Fun(Value), map_tree(Fun, Left), map_tree(Fun, Right)};

map_tree(Fun, Value) when is_number(Value) ->
    Fun(Value).

% Show it working
% rebar3 shell

% 1> T = binary_tree:new_node({60,binary_tree:new_node({20,10,binary_tree:new_node({40,70,nil})}),80}).

% {60,{20,10,{40,70,nil}},80}

% 2> functor:map_tree(fun(X) -> X * 2 end, T).

% {120,{40,20,{80,140,nil}},160}