-module(trie).
-export([
    empty/1, add/2, contains/2, remove/2, prefix/2, toList/1, fromList/1,
    new/0
]).


new() ->
    #{}.

empty(Trie) ->
    (maps:size(Trie) =:= 0) orelse (Trie =:= #{}).


add(Trie, Word) when is_list(Word) ->
    add_chars(Trie, Word);

add(Trie, Word) when is_atom(Word) ->
    add_chars(Trie, atom_to_list(Word)).

add_chars(Trie, []) ->
    Trie#{endatom => true};
add_chars(Trie, [H|T]) ->
    SubTrie = maps:get(H, Trie, #{}),
    Trie#{H => add_chars(SubTrie, T)}.


contains(_, []) ->
    false;
contains(Trie, [H]) ->
    case maps:get(H, Trie, undefined) of
        undefined -> false;
        Map -> maps:is_key(endatom, Map)
    end;
contains(Trie, [H|T]) ->
    case maps:get(H, Trie, undefined) of
        undefined -> false;
        SubTrie -> contains(SubTrie, T)
    end.


remove(Trie, []) ->
    maps:remove(endatom, Trie);
remove(Trie, [H|T]) ->
    case maps:get(H, Trie, undefined) of
        undefined -> Trie;
        SubTrie ->
            NewSub = remove(SubTrie, T),
            case empty(NewSub) of
                true -> maps:remove(H, Trie);
                false -> Trie#{H => NewSub}
            end
    end.


prefix(Trie, Prefix) ->
    case follow_prefix(Trie, Prefix) of
        undefined -> [];
        Node -> sequences_from(Node, Prefix)
    end.

follow_prefix(Trie, []) -> Trie;
follow_prefix(Trie, [H|T]) ->
    case maps:get(H, Trie, undefined) of
        undefined -> undefined;
        SubTrie -> follow_prefix(SubTrie, T)
    end.


toList(Trie) ->
    sequences_from(Trie, []).

sequences_from(Trie, Prefix) ->
    Words = case maps:is_key(endatom, Trie) of
        true -> [Prefix];
        false -> []
    end,
    Children = lists:foldl(
        fun({K, Sub}, Acc) when K =/= endatom ->
                Acc ++ sequences_from(Sub, Prefix ++ [K]);
           (_, Acc) -> Acc
        end, [], maps:to_list(Trie)),
    Words ++ Children.


fromList(List) ->
    lists:foldl(fun(Seq, Trie) -> add(Trie, Seq) end, new(), List).
