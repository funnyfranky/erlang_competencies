-module(monoid).
-author(josh).

-export([aandm_monoid/2]).

aandm_monoid(Element1,Element2) -> 
    ({lists:foldl(fun(X,Acc) -> X + Acc end, 0, Element1 ++ Element2),
    lists:foldl(fun(X,Acc) -> X * Acc end, 1, Element1 ++ Element2)}).