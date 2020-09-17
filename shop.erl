-module(shop).
-export([total/1, cost/1]).


total([{What,N}|T]) ->
	cost(What) * N + total(T);
total([]) -> 
	0.

cost(oranges) -> 5;
cost(apples) -> 25;
cost(pears) -> 2;
cost(milk) -> 7.
