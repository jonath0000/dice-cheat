%%%
%%% Module: dice_cheat
%%%
%%% Some cheat routines for dice games.
%%%
-module(dice_cheat).
-export([combo_prob/3]).


%%
%% Recursive probablility search, brute force style!
%%
%% Returns the probablility to find Combo given a number of 
%% Strokes left, where the previous stroke gave Val.
%% Sides determines number of sides on dice.
%%
combo_prob(Strokes, Val, Combo, Sides) ->

   if Strokes > 0 ->
      
      % Find Val in Combo and remove.
      Found = lists:any(fun(X) -> X == Val end, Combo),
      if Found ->         
	 ComboChild = lists:delete(Val, Combo);
      true ->
         ComboChild = Combo
      end,

      % If all Combo items found, return p=1.
      if length(ComboChild) == 0 ->
         1.0;
      % Else recurse and add partial probablilities...
      true ->
         lists:sum(lists:map(
            fun(X) -> combo_prob(Strokes-1, X, ComboChild, Sides)/Sides end, 
            lists:seq(1,Sides)))
      end;

   true ->
      0.0
   end.


%%
%% Probablility of a particular dice combo given
%% the number of dice strokes.
%%
%% Strokes - number of dice strokes to eval. 
%%          (eg. 10 will take ~1 min to calc.)
%% Combo   - list with dice combination to eval.
%% Sides   - Number of sides on dice.
%%
%% Example: combo_prob(5, [1, 1]) will give 
%% the probablility to find a pair of 1:s in
%% 5 strokes.
%%
combo_prob(Strokes, Combo, Sides) ->
   lists:sum(lists:map(
      fun(X) -> combo_prob(Strokes, X, Combo, Sides)/Sides end, 
      lists:seq(1,Sides))).
   









