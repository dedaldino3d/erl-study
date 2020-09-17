-module(geometry2).
-export([test/0,area/1]).


test() ->
    20 = area({retangule,2,10}),
    100 = area({square,10}),
    test_worked.

area({retangule, Weight, Height}) -> Weight * Height;
area({square,Side}) -> Side * Side.
