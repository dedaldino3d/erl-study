-module(math_functions).
-export([odd/1, even/1, filter/2]).


odd(X) -> X rem 2 =:= 0.
even(X) -> X rem 2 =:= 1.

filter(F, L) -> [X || X <- L, F(X)].
