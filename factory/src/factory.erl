-module(factory).

-export([make_rectangle/2]).

make_rectangle(Length, Breadth) ->
    Area = Length * Breadth,
    Perimeter = 2 * (Length + Breadth),
    #{type => rectangle,
      length => Length,
      breadth => Breadth,
      area => Area,
      perimeter => Perimeter}.