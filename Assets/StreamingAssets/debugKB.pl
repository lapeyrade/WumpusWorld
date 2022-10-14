:- include('kb.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_arrow(human, 1).
nb_gold(human, 0).
intelligence(human, 3).
strength(human, 5).
dexterity(human, 7).
personality(_, determinist).
personality(_, killer).
personality(_, greedy).
% ------------------
% (7, 4)(6, 4)(6, 3)(5, 3)(4, 3)(4, 2)(5, 2)(6, 2)(6, 1)(5, 1)(4, 1)(3, 1)(2, 1)(1, 1)(1, 1)
cell([1, 1], cell).
cell([1, 1], visited).
cell([2, 1], cell).
cell([2, 1], visited).
cell([3, 1], cell).
cell([3, 1], visited).
cell([4, 1], cell).
cell([4, 1], visited).
cell([5, 1], cell).
cell([5, 1], visited).
cell([6, 1], cell).
cell([6, 1], visited).
cell([7, 1], cell).
cell([7, 1], breezeyes).
cell([7, 1], visited).
cell([6, 2], cell).
cell([6, 2], breezeyes).
cell([6, 2], visited).
cell([5, 2], cell).
cell([5, 2], visited).
cell([4, 2], cell).
cell([4, 2], visited).
cell([3, 2], cell).
cell([3, 2], breezeyes).
cell([3, 2], visited).
cell([2, 2], cell).
cell([2, 2], breezeyes).
cell([2, 2], visited).
cell([1, 2], cell).
cell([1, 2], visited).
cell([0, 2], cell).
cell([0, 2], wall).
cell([1, 3], cell).
cell([1, 3], breezeyes).
cell([1, 3], visited).
cell([4, 3], cell).
cell([4, 3], breezeyes).
cell([4, 3], visited).
cell([5, 3], cell).
cell([5, 3], stenchyes).
cell([5, 3], visited).
cell([5, 4], wumpusdead).
cell([6, 3], cell).
cell([6, 3], visited).
cell([7, 3], cell).
cell([7, 3], breezeyes).
cell([7, 3], visited).
cell([6, 4], cell).
cell([6, 4], stenchyes).
cell([6, 4], visited).
cell([7, 4], cell).
cell([7, 4], visited).
cell([8, 4], cell).
cell([8, 4], wall).
cell([7, 4], human).
cell([7, 5], cell).
cell([7, 5], visited).
cell([8, 5], cell).
cell([8, 5], wall).
cell([6, 5], cell).
cell([6, 5], visited).
cell([5, 5], cell).
cell([5, 5], stenchyes).
cell([5, 5], visited).
cell([5, 6], cell).
cell([5, 6], breezeyes).
cell([5, 6], visited).
cell([6, 6], cell).
cell([6, 6], visited).
cell([7, 6], cell).
cell([7, 6], stenchyes).
cell([7, 6], visited).
cell([6, 7], cell).
cell([6, 7], stenchyes).
cell([6, 7], visited).
cell([5, 7], cell).
cell([5, 7], visited).
cell([4, 7], cell).
cell([4, 7], breezeyes).
cell([4, 7], visited).
cell([5, 8], cell).
cell([5, 8], wall).
cell([5, 7], human).
cell([5, 4], cell).
cell([5, 4], visited).
cell([4, 4], cell).
cell([4, 4], stenchyes).
cell([4, 4], visited).
