-module(compare_map).
-export([compare/2]).


compare(M1, M2) -> 
    erlang:is_map(M1) , erlang:is_map(M2),
    case maps:size(M1) < maps:size(M2) of
        false -> maps:to_list(M1) < maps:to_list(M2);
        true -> true
    end.
