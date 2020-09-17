-module(lib_misc).
-export([for/3, sum/1, map/2, qsort/1, pythag/1, perms/1, odds_and_evens/1, sleep/1]).
-import(lists, [seq/2]).
-author("Dedaldino António").
-vsn("0.1.0").


-spec sleep(N) -> when
  N :: integer().



sleep(T) ->
  receive
  after T ->
    true
  end.


for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I) | for(I + 1, Max, F)].

sum([H | T]) -> H + sum(T);
sum([]) -> 0.

map(_, []) -> [];
map(F, [H | T]) -> [F(H) | map(F, T)];
map(F, L) -> [F(H) || H <- L].

qsort([]) -> [];
qsort([Pivot | T]) ->
  qsort([X || X <- T, X < Pivot])
  ++ [Pivot] ++
    qsort([X || X <- T, X >= Pivot]).


%% Pythagorian tripplets, find from a sequence of integers(N) 
% equals to A^2 + B^2 = C^2 
% and A + B + C equal or less than N(integer)
pythag(N) ->
  [{A, B, C} ||
    A <- seq(1, N),
    B <- seq(1, N),
    C <- seq(1, N),
    A + B + C =< N,
    A * A + B * B =:= C * C
  ].

%% Find permutation

perms([]) -> [[]];
perms(L) -> [[H | T] || H <- L, T <- perms(L--[H])].


%% Compute if a integer is odd or even

odds_and_evens(L) ->
  odds_and_evens_ccc(L, [], []).

odds_and_evens_ccc([H | T], Odds, Evens) ->
  case (H rem 2) of
    1 -> odds_and_evens_ccc(T, [H | Odds], Evens);
    0 -> odds_and_evens_ccc(T, Odds, [H | Evens])
  end;

odds_and_evens_ccc([], Odds, Evens) ->
  {lists:reverse(Odds), lists:reverse(Evens)}.

%% First Guard

% A = -1.

% if
%     A > 0 ->
%         worked_fine
%     true ->
%         not_at_all
% end

% f(X) when is_integer(X), x =:= 0 -> is_greater_than_0.

% Currying, like lambda:
% [lambda x: x*x ] Python
% CurF = fun(L) -> 
%     fun(L2) -> 
%         lists:reverse(lists:map(fun(X) -> 
%             [H * X * 2 || H <- L2, H =< 5] end,
%             L)
%         )
%         end 
%     end

consult(File) ->
  case file:open(File, read) of
    {ok, S} ->
      Result = consultF(S),
      file:close(S),
      {ok, Result};
    {error, Why} ->
      {error, Why}
  end.

consultF(S) ->
  case io:read(S, '') of
    {ok, Term} -> [Term|consultF(S)];
    eof -> [];
    Error ->Error
end.
