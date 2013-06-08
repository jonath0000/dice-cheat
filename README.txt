Erlang lib with some dice game cheat routines (calculating probabilities etc.)
Made mostly for learning the Erlang language.

Example:

To get the probability for getting a pair of "ones" from rolling 5 dices:

     dice_cheat:combo_prob(5, [1,1]).
     >> 0.1962448559670782

To get the probablity to get a "small straight" from rolling 5 dices:

     dice_cheat:combo_prob(5, [1,2,3,4,5]).
     >> 0.015432098765432098


Updated version:

     dice:p([1,1,1,1,1], 15).
     >> 0.08976566741930443

This one is much faster using some cool optimizations :)

