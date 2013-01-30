
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
    {Res, _} = p(Outcome, N, K, dict:new()),
    Res.
%% The dict is a cache of previously computed probabilities.

%% p/2

p(Values, N) ->
    p(Values, N, ?DICE).

%% ===========================================================================

%% p/4

%% Fewer rolls than the length of the outcome.
p({Len,_}, N, _, Cache)
  when N < Len ->
    {0.0, Cache};

%% Any result (including length 0) has an outcome of length 0.
p({0,_}, _, _, Cache) ->
    {1.0, Cache};

p({_,X} = T, N, K, Cache0) ->
    case dict:find({X,N}, Cache0) of
        {ok, Prob} ->
            {Prob, Cache0};
        error ->
            {Prob, Cache} = compute(T, N, K, Cache0),
            {Prob, dict:store({X,N}, Prob, Cache)}
    end.

%% P(X,n) = C(X,n) / k^n
compute({N,X}, N, K, Cache) ->
    {try count(X, N, K) of
         Count ->
             quot(Count, K, N)
     catch
         ?MODULE -> 0.0
     end,
     Cache};

%% P(X,n) = 1/k * [\sum_{j=1}^m P(X_j,n-1) + (k - m) * P(X,n-1)]
compute({_,X} = T, N, K, Cache0) ->
    M = length(X),
    N1 = N - 1,
    {P0, Cache1} = p(T, N1, K, Cache0),
    {Sum, Cache} = lists:foldl(fun({J,_}, {S,C0}) ->
                                       {P,C} = p(delete(J,T), N1, K, C0),
                                       {S+P, C}
                               end,
                               {(K - M) * P0, Cache1},
                               X),
    {Sum/K, Cache}.

%% count/3
%%
%% Compute C(X,n) = n! / (c_1 * ... * c_m)

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
