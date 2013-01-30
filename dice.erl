
-module(dice).

%%
%% Compute P(X,n) as defined in dice.txt.
%%

-export([p/2,
         p/3]).

%% The number of sides on regular dice.
-define(DICE, 6).

%% p/3
%%
%% Return the probability of obtaining each of a list of values with N
%% rolls of a fair K-sided die.

p(Values, N, K)
  when is_list(Values) ->
    p(outcome(Values), N, K);

p(Outcome, N, K)
  when is_integer(N), is_integer(K), 0 < K ->
    prob(Outcome, N, K).

%% p/2

p(Values, N) ->
    p(Values, N, ?DICE).

%% ===========================================================================

%% prob/3

%% Fewer rolls than the length of the outcome.
prob({Len,_}, N, _)
  when N < Len ->
    0.0;

%% Any result (including length 0) has an outcome of length 0.
prob({0,_}, _, _) ->
    1.0;

%% Number of rolls = length of outcome.
prob({N,X}, N, K) ->
    try count(X, N, K) of
        Count -> quot(Count, K, N)
    catch
        ?MODULE -> 0.0
    end;

prob({_,X} = T, N, K) ->
    M = length(X),
    N1 = N - 1,
    lists:foldl(fun({J,_}, A) -> A + prob(delete(J,T), N1, K) end,
                (K - M) * prob(T, N-1, K),
                X)
        / K.

count(X, N, K) ->
    lists:foldl(fun({J,C},A) when 1 =< J, J =< K -> A div fact(C);
                   (_,_) -> throw(?MODULE)  %% not a die value
                end,
                fact(N),
                X).

%% ===========================================================================

quot(Res, _, 0) ->
    Res;
quot(Acc, K, N) ->
    quot(Acc/K, K, N-1).

%% Represent an outcome as pair of a length and an ordered list of
%% value/occurences pairs.
outcome(Values) ->
    lists:foldl(fun(J,{L,D}) -> {L+1, orddict:update_counter(J, 1, D)} end,
                {0, orddict:new()},
                Values).

%% Remove a single integer from an outcome.
delete(N, {Len, X} = T) ->
    case orddict:find(N, X) of
        {_, 1} ->
            {Len - 1, orddict:erase(N, X)};
        {_, _} ->
            {Len - 1, orddict:update_counter(N, -1, X)};
        false ->
            T
    end.

fact(N) ->
    fact(N, 1).

fact(0, Res) ->
    Res;
fact(N, Acc) ->
    fact(N-1, N*Acc).
