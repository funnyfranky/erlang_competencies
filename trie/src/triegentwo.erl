-module(triegentwo).
-export([empty/1, new/0, add/2, contains/2, remove/2, prefix/2, toList/1, fromList/1]).
-include_lib("eunit/include/eunit.hrl").

%%%  Constructor
new() ->
    #{}.

%%% 1. empty(trie)
empty(Trie) ->
    (maps:size(Trie) =:= 0) orelse (Trie =:= #{}).

%%% 2. add(trie, sequence)
add(Trie, []) ->
    Trie#{endatom => true};
add(Trie, [H|T]) ->
    SubTrie = maps:get(H, Trie, #{}),
    Trie#{H => add(SubTrie, T)}.

%%% 3. contains(trie, sequence)
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

%%% 4. remove(trie, sequence)
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

%%% 5. prefix(trie, prefix)
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

%%% 6. toList(trie)
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

%%% 7. fromList(list)
fromList(List) ->
    lists:foldl(fun(Seq, Trie) -> add(Trie, Seq) end, new(), List).

%%% TESTS
empty_test() ->
    ?assert(triegentwo:empty(triegentwo:new())).

add_contains_test() ->
    Trie = triegentwo:add(triegentwo:new(), "cat"),
    ?assert(triegentwo:contains(Trie, "cat")),
    ?assert(not triegentwo:contains(Trie, "ca")).

duplicate_test() ->
    Trie = triegentwo:add(triegentwo:add(triegentwo:new(), "cat"), "cat"),
    ?assert(triegentwo:contains(Trie, "cat")),
    ?assertEqual(["cat"], triegentwo:toList(Trie)).

remove_test() ->
    Trie = triegentwo:fromList(["cat","car","dog"]),
    Trie2 = triegentwo:remove(Trie, "car"),
    ?assert(not triegentwo:contains(Trie2, "car")),
    ?assert(triegentwo:contains(Trie2, "cat")),
    ?assert(triegentwo:contains(Trie2, "dog")).

prefix_test() ->
    Trie = triegentwo:fromList(["car","cat","cow","dog"]),
    ?assertEqual(["car","cat"], lists:sort(triegentwo:prefix(Trie, "ca"))),
    ?assertEqual([], triegentwo:prefix(Trie, "z")).

to_from_list_test() ->
    Words = ["a","ab","abc","b"],
    Trie = triegentwo:fromList(Words),
    ?assertEqual(lists:sort(Words), lists:sort(triegentwo:toList(Trie))).
