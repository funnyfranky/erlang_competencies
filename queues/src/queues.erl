-module(queues).

-export([empty/1, enqueue/2, dequeue/1, head/1, tail/1, toList/1, fromList/1]).

% In your editor, implement a functional queue using the structure {list, list}. The queue should support two lists: one for the front elements and another for the rear elements. Implement the following functions:

% empty(queue)
% Input: A queue
% Output: true if the queue is empty, false otherwise

% enqueue(queue, element)
% Input: A queue and an element
% Output: A new queue with the element added to the rear

% dequeue(queue)
% Input: A queue
% Output: A new queue with the first element removed

% head(queue)
% Input: A queue
% Output: The first element or nil if the queue is empty

% tail(queue)
% Input: A queue
% Output: The last element or nil if the queue is empty

% toList(queue)
% Input: A queue
% Output: A list of elements in order

% fromList(list)
% Input: A list of elements
% Output: A queue


% Task Instructions:
% Use functional programming principles to implement these functions.
% Write tests to validate the correctness of each function.
% Ensure the queue adheres to the FIFO principle.
% Use recursive definitions wherever appropriate.
% Provide a test suite with multiple scenarios, including edge cases, such as:
% Enqueuing and dequeuing elements in an empty queue
% Accessing the head and tail of the queue at various stages
% Converting between lists and queues
% Combining multiple operations, such as enqueue and dequeue, sequentially

empty({[],[]}) -> true;
empty(_) -> false.

enqueue({[],[]}, Element) ->
    {[],[Element]};
enqueue({Front,[]}, Element) when is_list(Front) ->
    {Front,[Element]};
enqueue({[],Rear}, Element) when is_list(Rear) ->
    {[],[Element | Rear]};
enqueue({Front,Rear}, Element) when is_list(Rear) and is_list(Front) ->
    {Front,[Element | Rear]}.

dequeue({[],[]}) ->
    {nil, {[],[]}};
dequeue({[],Rear}) when is_list(Rear) ->
    Head = lists:reverse(Rear),
    [Dequeued | T] = Head,
    {Dequeued, {T,[]}};
dequeue({Front,Rear}) when is_list(Front) ->
    [Dequeued | T] = Front,
    {Dequeued, {T,Rear}}.

head({[],[]}) ->
    nil;
head({[],Rear}) when is_list(Rear) ->
    [Head | _Tail] = lists:reverse(Rear),
    Head;
head({Front, _}) when is_list(Front) ->
    [Head | _] = Front,
    Head.

tail({[],[]}) ->
    nil;
tail({Front,[]}) when is_list(Front) ->
    [H | _T] = lists:reverse(Front),
    H;
tail({_,Rear}) when is_list(Rear) ->
    [H | _T] = Rear,
    H.

toList({[],[]}) ->
    [];
toList({[],Rear}) when is_list(Rear) ->
    lists:reverse(Rear);
toList({Front,[]}) when is_list(Front) ->
    Front;
toList({Front,Rear}) when is_list(Front) and is_list(Rear) ->
    Front ++ lists:reverse(Rear).

fromList(List) when is_list(List) ->
    {List,[]}.