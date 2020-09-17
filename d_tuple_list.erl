-module(d_tuple_list).
-export([tuple_to_list/1]).


tuple_to_list(T) ->
    [{N,number} || {N, number} <- T];

tuple_to_list(T) -> 
    if is_tuple(T),
    