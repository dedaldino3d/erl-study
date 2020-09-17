-module(area_server_final).
-export([loop/0, area/2, start/0]).


start() -> spawn(area_server_final, loop, []).

area(Pid, What) ->
    rpc(Pid, What).

% remote procedure call
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
        {From, {square, Side}} ->
            From ! {self(), Side * Side},
            loop();
        {From, {circle, R}} ->
            From ! {self(), 3.14159 * R * R},
            loop();
        % match all messages not included in previous patterns
        {From, Other} ->
            From ! {self(), {error, Other}},
            loop()
    end.
