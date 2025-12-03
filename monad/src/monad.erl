-module(monad).

-export([
    fun_unit/1,
    fun_bind/2,
    test/0
]).

fun_unit(Value) ->
    {Value, []}.

fun_bind({Value, Log1}, Fun) ->
    {NewValue, Log2} = Fun(Value),
    {NewValue, Log1 ++ Log2}.







add2(X) ->
    {X + 2, ["added 2"]}.

mul3(X) ->
    {X * 3, ["multiplied by 3"]}.

sub5(X) ->
    {X - 5, ["subtracted 5"]}.

test() ->
    Start = fun_unit(4),
    Step1 = fun_bind(Start, fun add2/1),  %% {6,  ["added 2"]}
    Step2 = fun_bind(Step1, fun mul3/1),  %% {18, ["added 2","multiplied by 3"]}
    Result = fun_bind(Step2, fun sub5/1), %% {13, ["added 2","multiplied by 3","subtracted 5"]}
    Result.
