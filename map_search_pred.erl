-module(map_search_pred).
-export([search_pred/2]).


search_pred(Map, Pred) ->
    if
        erlang:is_map(Map), erlang:is_function(Pred) ->
            M = maps:to_list(Map),
            maps:from_list([{K, V} || {K, V} <- M, Pred(K, V)]);

        true -> "Map is not a map or Pred is not a function"
    end.

% search_pred(Map) ->
%     if 
%         erlang:is_map(Map) ->
%             M = maps:to_list(Map),
%             maps:from_list([{K,V} || {K,V} <- M, erlang:is_integer(V), V =:= 7 ]);
%         true -> "give a map."
%     end.
