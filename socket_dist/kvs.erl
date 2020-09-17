-module(kvs).
-export([start/0, store/2, lookup/1]).


-spec kvs:start() -> boolean().
-spec kvs:store(Key::any(), Value::any()) -> boolean().
-spec kvs:lookup(Key::any()) -> {ok, Value::any()} | undefined.


start() -> register(kvs, spawn(fun() -> loop() end)).

store(Key, Value) -> rpc({store, Key, Value}).

rpc(Request) ->
    kvs ! {self(), Request},
    receive
        {kvs, Response} ->
            Response
    end.

lookup(Key) -> rpc({lookup, Key}).

loop() ->
    receive
        {From, {store, Key, Value}} ->
            put(Key, {ok, Value}),
            From ! {kvs, true},
            loop();
        {From, {lookup, Key}} ->
            From ! {kvs, get(Key)},
            loop()
    end.
