% In your editor, implement the following lazy computation scenario:

% Create a lazy computation that generates an infinite stream of data, 
% one at a time, based on a recursive formula. Test it with a formula 
% like `x(n) = x(n-1) + 2`.

-module(lazy_comp).

-export([start/1, next/1]).

start(X0) ->
    spawn(fun() -> loop(X0) end).
    
next(Pid) ->
    Pid ! {self(), next},
    receive
        {Pid, Value} -> Value
    end.
        
loop(Current) ->
    receive
        {From, next} ->
            Next = Current + 2,

            From ! {self(), Next},
            
            loop(Next)
    end.