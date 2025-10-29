queues
=====

An OTP library

Build
-----

    $ rebar3 compile


queues have an amortized time complexity of big O of 1.

A queue is formed as a two tuple that is comprised of two lists. there are no nodes and no pointers to the next in queue, so all structure and manipulation is based off manipulating the lists using list operations prepend, append and reverse.

the empty function checks if both lists are empty, returning true if they are and false if not. the head function retrieves the head of the first list, which is the first item in the queue, without removing it. the tail function peeks at the last item added in the queue (which is at the head element of the second list), without removing it. Enqueue adds a new item to the head of the second list, adding it to the logical end of the queue. Dequeue removes and returns the head element of the first list which is the logical head of the queue. If the first list is empty, it will reverse the items in the second list and that becomes the first list, emptying the second list.