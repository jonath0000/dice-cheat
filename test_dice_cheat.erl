%%%
%%% test_dice_cheat
%%%
%%% Test cases for dice_cheat.
%%%
-module(test_dice_cheat).
-export([run_tests/0]).

%%
%% Assert value is inside range.
%%
assert_in(Val, LowBound, HiBound) ->
   if (Val > LowBound) and (Val < HiBound)  ->
      true;
   true ->
      io:format("Test FAIL: ~w not within ~w to ~w~n",[Val, LowBound, HiBound])
   end.

%%
%% Ref. data generated from
%% http://blog.plover.com/math/yahtzee.html
%% http://wizardofodds.com/ask-the-wizard/probability/dice/
%% ...and by some manual calculations.
%%
run_tests() ->

   % Check prob. to get all same in N number of strokes.
   assert_in(dice_cheat:combo_prob(2,[2,2],6)*6, 0.166, 0.167),
   assert_in(dice_cheat:combo_prob(3,[3,3,3],6)*6, 0.027, 0.028),	
   assert_in(dice_cheat:combo_prob(4,[4,4,4,4],6)*6, 0.00462, 0.00464),	
   assert_in(dice_cheat:combo_prob(5,[6,6,6,6,6],6)*6, 0.000771, 0.000773),	

   % Check prob. to get certain number in N strokes.
   assert_in(dice_cheat:combo_prob(1,[1],6), 0.166, 0.167),
   assert_in(dice_cheat:combo_prob(2,[3],6), 11/36-0.0001, 11/36+0.0001),
   assert_in(dice_cheat:combo_prob(3,[2],6), 91/216-0.0001, 91/216+0.0001),
   assert_in(dice_cheat:combo_prob(4,[1],6), 671/1296-0.0001, 671/1296+0.0001),

   % Check prob. to get a particular pair in N strokes.
   assert_in(dice_cheat:combo_prob(3,[5,5],6), 0.074, 0.075),
   assert_in(dice_cheat:combo_prob(4,[5,5],6), 
      (6*25+4*5+1)/1296-0.001, 
      (6*25+4*5+1)/1296+0.001),

   % Check prob. of straights.
   assert_in(dice_cheat:combo_prob(5,[1,2,3,4,5],6), 120/7776-0.001, 120/7776+0.001),
   assert_in(dice_cheat:combo_prob(5,[2,3,4,5,6],6), 120/7776-0.001, 120/7776+0.001),
   true.


