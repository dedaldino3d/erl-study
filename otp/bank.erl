-module(bank).
-export([start/0, stop/0, new_account/1, deposit/2,withdraw/2]).
-vsn("0.1").
-author({dedaldino, antonio}).


% gen_server behaviour
-behaviour(gen_server).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
        handle_info/2, terminate/2, code_change/3]).
-compile(export_all).

-ifndef(SERVER).
-define(SERVER, ?MODULE).
-endif.

% -spec start() -> link().
start() -> gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() -> gen_server:call(?MODULE, stop).

new_account(Who) -> gen_server:call(?MODULE, {new, Who}).
deposit(Who, Amount) -> gen_server:call(?MODULE, {add, Who, Amount}).
withdraw(Who, Amount) -> gen_server:call(?MODULE, {remove, Who, Amount}).


% implementation of gen_server callbacks
init([]) -> {ok, ets:new(?MODULE, [])}.

handle_call({new, Who}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> ets:insert(Tab, Who),
              {welcome, Who};
            [_] -> {Who, you_are_already_a_customer}
          end,
  {reply, Reply, Tab};


handle_call({add, Who, Amount}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> you_are_not_a_customer;
            [{Who, Balance}] ->
              NewBalance = Balance + Amount,
              ets:insert(Tab, Who, NewBalance),
              {thanks, Who, you_balance_is, NewBalance}
          end,
  {reply, Reply, Tab};

handle_call({remove, Who, Amount}, _From, Tab) ->
  Reply = case ets:lookup(Tab, Who) of
            [] -> {sorry, you_are_not_a_customer};
            [{Who, Balance}] ->
              if
                Balance > Amount, Balance - Amount >= 5 ->
                  NewBalance = Balance - Amount,
                  ets:insert(Tab, Who, NewBalance),
                  {sad, you_balance_is, NewBalance};
                true ->
                  {sorry, not_enough_money}
              end
          end,
  {reply, Reply, Tab};

handle_call(stop, _From, Tab) ->
  {stop, normal, stopped, Tab}.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {info, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
