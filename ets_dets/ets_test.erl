-module(ets_test).
-export([start/0]).


start() ->
    lists:foreach(fun test_ets/1, [set. ordered_set, bag, duplicate_bag]).


-spec test_ets(Mode) -> ok when
    Mode:: set | ordered_set | bag | duplicate_bag.

test_ets(Mode) ->
    TableId = ets:new(test, [Mode]),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {b,2}),
    ets:insert(TableId, {a,1}),
    ets:insert(TableId, {c,5}),
    List = ets:table2list(TableId),
    io:format("~-13w  =>  ~p~n", [Mode, List]),
    ets:delete(TableId),
    io:format("Table ~p was deleted ~n", [TableId]),
    ok.
