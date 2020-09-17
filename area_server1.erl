-module(area_server1).
-export([loop/0, rpc/2]).


rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} ->
            Response
    end.

loop() ->
    receive
        {From, {retangule, Weigth, Heigth}} ->
            From ! {self(), Weigth * Heigth},
            loop();
        {From, {circle, R}} ->
            From ! {self(), 3.14159 * R *R},
            loop();
        {From, {square, Side}} ->
            From !{self(), Side * Side},
            loop();
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.
