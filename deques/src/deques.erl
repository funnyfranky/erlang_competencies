-module(deques).

-export([empty/1, enqueue/2, enqueue_front/2, dequeue/1, dequeue_back/1, head/1, tail/1, toList/1, fromList/1]).

% In your editor, implement a functional deque (double-ended queue) using the structure {list, list}. The deque should consist of two lists: one for the front elements and another for the rear elements. Implement the following functions:

% empty(deque): Checks if the deque is empty.
% Input: A deque.
% Output: true if the deque is empty, false otherwise.

% enqueue(deque, element): Adds an element to the rear of the deque.
% Input: A deque and an element.
% Output: A new deque with the element added to the rear.

% enqueue_front(deque, element): Adds an element to the front of the deque.
% Input: A deque and an element.
% Output: A new deque with the element added to the front.

% dequeue(deque): Removes the first element from the deque.
% Input: A deque.
% Output: A new deque with the first element removed.

% dequeue_back(deque): Removes the last element from the deque.
% Input: A deque.
% Output: A new deque with the last element removed.

% head(deque): Retrieves the first element of the deque without removing it.
% Input: A deque.
% Output: The first element or nil if the deque is empty.

% tail(deque): Retrieves the last element of the deque without removing it.
% Input: A deque.
% Output: The last element or nil if the deque is empty.

% toList(deque): Converts the deque into a single list of elements.
% Input: A deque.
% Output: A list of elements in order.

% fromList(list): Converts a list of elements into a deque.
% Input: A list of elements.
% Output: A deque.


% Write tests to validate the correctness of each function. Include multiple scenarios, such as:
% Enqueuing and dequeuing elements from both ends in an empty deque.
% Accessing the head and tail of the deque at various stages.
% Converting between lists and deques.
% Combining multiple operations, such as enqueue, dequeue, and enqueue_front, sequentially.

% Challenge Extension (Optional):
% Implement a reverse(deque) function that reverses the order of elements in the deque.
% Implement a length(deque) function to count the elements in the deque.
% Write a merge(deque1, deque2) function that combines two deques into one.


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

enqueue_front({[],[]}, Element) ->
    {[Element],[]};
enqueue_front({Front,Rear}, Element) when is_list(Front) and is_list(Rear) ->
    {[Element | Front],Rear}.

dequeue({[],[]}) ->
    {nil, {[],[]}};
dequeue({[],Rear}) when is_list(Rear) ->
    Head = lists:reverse(Rear),
    [Dequeued | T] = Head,
    {Dequeued, {T,[]}};
dequeue({Front,Rear}) when is_list(Front) ->
    [Dequeued | T] = Front,
    {Dequeued, {T,Rear}}.

dequeue_back({[],[]}) ->
    {[],[]};
dequeue_back({Front,[]}) when is_list(Front) ->
    [H | ReverseFront] = lists:reverse(Front),
    {H, {lists:reverse(ReverseFront),[]}};
dequeue_back({Front,Back}) when is_list(Back) ->
    [H | T] = Back,
    {H, {Front,T}}.


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