%%%
%%% test_dice_cheat
%%%
%%% Test cases for dice_cheat.
%%%
-module(test_dice_cheat).

-export([run_tests/0,
         run1/1]).

-define(TESTCASES,
        [% Check prob. to get all same in N number of strokes.
         {2, [2,2], 6, 0.166, 0.167},
         {3, [3,3,3], 6, 0.027, 0.028},
         {4, [4,4,4,4], 6, 0.00462, 0.00464},
         {5, [6,6,6,6,6], 6, 0.000771, 0.000773},
         % Check prob. to get certain number in N strokes.
         {1, [1], 0.166, 0.167},
         {2, [3], 11/36-0.0001, 11/36+0.0001},
         {3, [2], 91/216-0.0001, 91/216+0.0001},
         {4, [1], 671/1296-0.0001, 671/1296+0.0001},
         % Check prob. to get a particular pair in N strokes.
         {3, [5,5], 0.074, 0.075},
         {4, [5,5], (6*25+4*5+1)/1296-0.001, (6*25+4*5+1)/1296+0.001},
         % Check prob. of straights.
         {5, [1,2,3,4,5], 120/7776-0.001, 120/7776+0.001},
         {5, [2,3,4,5,6], 120/7776-0.001, 120/7776+0.001}]).


%%
%% Assert value is inside range.
%%
assert_in(Val, LowBound, HiBound)
  when LowBound < Val, Val < HiBound  ->
    true;

assert_in(Val, LowBound, HiBound) ->
    io:format("Test FAIL: ~w not within ~w to ~w~n",[Val, LowBound, HiBound]).

%%
%% Ref. data generated from
%% http://blog.plover.com/math/yahtzee.html
%% http://wizardofodds.com/ask-the-wizard/probability/dice/
%% ...and by some manual calculations.
%%
run_tests() ->
   lists:foreach(fun run1/1, ?TESTCASES).

run1({Strokes, Combo, Lo, Hi}) ->
    run1({Strokes, Combo, 1, Lo, Hi});

run1({Strokes, Combo, N, Lo, Hi}) ->
   assert_in(dice_cheat:combo_prob(Strokes, Combo) * N, Lo, Hi).
