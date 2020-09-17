-module(clock).
-export([start/2, stop/1]).


-spec start(T::integer(), F::func(), _Type::binary()) -> pid().


start(T, F, _Type) -> 
    register(clock, spawn(fun() -> tick(T, F) end));
    case _Type of
        clocker ->
            clock ! io:format("TICK ~p~n", [erlang:now()]);
        _ ->
            void
    end.

stop() -> clock ! stop.

tick(T,F) ->
    receive
        stop ->
            void;
    after T ->
        F();
        tick(T, Fun)
    end.
