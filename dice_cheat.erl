%%%
%%% Module: dice_cheat
%%%
%%% Some cheat routines for dice games (6-sided).
%%%
-module(dice_cheat).
-export([combo_prob/2]).


%%
%% Recursive probablility search, brute force style!
%%
%% Returns the probablility to find Combo given a number of 
%% Strokes left, where the previous stroke gave Val.
%%
combo_prob(Strokes, Val, Combo)
  when Strokes > 0 ->
      
      %io:format("combo_prob times=~w val=~w~n",[Strokes, Val]),

      % Find Val in Combo and remove.
      ComboChild = lists:delete(Val, Combo);

      % If all Combo items found, return p=1.
      if length(ComboChild) == 0 ->
         1.0;
      % Else recurse and add partial probablilities...
      true ->
        (combo_prob(Strokes-1, 1, ComboChild) +
      	 combo_prob(Strokes-1, 2, ComboChild) +
         combo_prob(Strokes-1, 3, ComboChild) +
         combo_prob(Strokes-1, 4, ComboChild) +
         combo_prob(Strokes-1, 5, ComboChild) +
         combo_prob(Strokes-1, 6, ComboChild)) / 6
      end;

combo_prob(_Strokes, _Val, _Combo) ->
      0.0.


%%
%% Probablility of a particular dice combo given
%% the number of dice strokes.
%%
%% Strokes - number of dice strokes to eval. 
%%          (eg. 10 will take ~1 min to calc.)
%% Combo   - list with dice combination to eval.
%%
%% Example: combo_prob(5, [1, 1]) will give 
%% the probablility to find a pair of 1:s in
%% 5 strokes.
%%
combo_prob(Strokes, Combo) ->
  (combo_prob(Strokes, 1, Combo) + 
   combo_prob(Strokes, 2, Combo) + 
   combo_prob(Strokes, 3, Combo) + 
   combo_prob(Strokes, 4, Combo) + 
   combo_prob(Strokes, 5, Combo) + 
   combo_prob(Strokes, 6, Combo)) / 6.




