-module(monad).

-export([
    unit/1,
    bind/2,
    test/0
]).

unit(Value) ->
    {Value, []}.

bind({Value, Log1}, Fun) ->
    {NewValue, Log2} = Fun(Value),
    {NewValue, Log1 ++ Log2}.







add2(X) ->
    {X + 2, ["added 2"]}.

mul3(X) ->
    {X * 3, ["multiplied by 3"]}.

sub5(X) ->
    {X - 5, ["subtracted 5"]}.

test() ->
    One = unit(4),
    Two = bind(One, fun add2/1),  %% {6,  ["added 2"]}
    Three = bind(Two, fun mul3/1),  %% {18, ["added 2","multiplied by 3"]}
    Result = bind(Three, fun sub5/1), %% {13, ["added 2","multiplied by 3","subtracted 5"]}
    Result.
