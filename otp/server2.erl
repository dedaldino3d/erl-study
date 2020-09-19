-module(server2).
-export([start/2, rpc/2]).
-doc(<<
    "A server that crashes the client if the query in the server results in an
    exception"
    >>).
-vsn("2.0").
-author({dedaldino, antonio}).


start(Name, Mod) ->
  register(Name, spawn(fun() -> loop(Name, Mod, Mod:init()) end)).


rp(Name, Request) ->
  Name ! {self, Request},
  receive 
    {Name, crash} -> exit(rpc);
    {Name, ok, Response} -> Response
  end.


loop(Name, Mod, OldState) ->
  receive
    {From, Request} ->
      try Mod:handle(Request, OldState) of
        {Request, NewState} -> 
          From ! {Name, ok, Response},
          loop(Name, Mod, NewState)
      catch _:Why ->
        log_error(Name, Request, Why);
        % send message to cause the client to crash
        From ! {Name, crash, Request},
        % loop with the original state
        loop(Name, Mod, OldState)
      end,
  end.


log_error(Name, Request, _Why) ->
  io:format("Server ~p request ~p ~n caused exception ~p~n", [Name, Request, _Why])

