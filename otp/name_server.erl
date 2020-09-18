-module(name_server).
-export([init/0, add/2, find/1, handle/2]).
-import(server1, [rpc/2]).
-doc(<<
    "A simple example of how OTP works, type: client"
    >>).
-vsn("1.0").
-author({dedaldino, antonio}).


% % client routines
add(Name, Place) -> rpc(name_server, {add, Name, Place}).
find(Name) -> rpc(name_server, {find, Name}).

% % callbacks routines
init() -> dict:new().
handle({add, Name, Place}, Dict) -> {ok, dict:store(Name, Place, Dict)};
handle({find, Name}, Dict) -> {dict:find(Name, Dict), Dict}.

