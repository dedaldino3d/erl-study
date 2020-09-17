-module(stimer).
-export([start/2, cancel/1]).

% Fun is a function not a pid
-spec start(T::interger(), Fun::pid()) -> pid().

-spec cancel(Pid::pid() -> atom().

start(T, Fun) -> spawn(fun() -> timer(T, Fun) end).

cancel(Pid) -> Pid ! cancel.

timer(T, Fun) ->
    receive
        cancel ->
            void;
    after T ->
        Fun()
    end.
