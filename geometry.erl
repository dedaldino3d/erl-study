-module(geometry).
-export([area/1]).


area({retangule, Weight, Height}) -> Weight * Height;
area({square, Side}) -> Side * Side.
