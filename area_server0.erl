-module(area_server0).
-export([loop/0]).
-author({dedaldino,antonio}).


loop() ->
    receive
      {retangule, Width, Heigth} -> 
          io:format("Area of retangule is ~p~n", [Width*Heigth]),
          loop();
      {square, Width} ->
          io:format("Area of square is ~p~n", [Width*Width]),
          loop()
    end.
