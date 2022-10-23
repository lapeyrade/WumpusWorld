:- include('kb.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_arrow(human0, 8).
nb_gold(human0, 0).
intelligence(human0, 3).
strength(human0, 5).
dexterity(human0, 7).
personality(human0, determinist).
personality(human0, killer).
personality(human0, greedy).
% ------------------
% (7, 3)(6, 3)(5, 3)(4, 3)(3, 3)(2, 3)(1, 3)(1, 2)(2, 2)(3, 2)(4, 2)(5, 2)(6, 2)(6, 1)(5, 1)(4, 1)(3, 1)(2, 1)(1, 1)(1, 1)
cell(cell, [1, 1]).
cell(visited, [1, 1]).
cell(cell, [2, 1]).
cell(visited, [2, 1]).
cell(cell, [3, 1]).
cell(visited, [3, 1]).
cell(cell, [4, 1]).
cell(visited, [4, 1]).
cell(cell, [5, 1]).
cell(visited, [5, 1]).
cell(cell, [6, 1]).
cell(visited, [6, 1]).
cell(cell, [7, 1]).
cell(stenchyes, [7, 1]).
cell(visited, [7, 1]).
cell(cell, [6, 2]).
cell(stenchyes, [6, 2]).
cell(visited, [6, 2]).
cell(cell, [5, 2]).
cell(visited, [5, 2]).
cell(cell, [4, 2]).
cell(visited, [4, 2]).
cell(cell, [3, 2]).
cell(visited, [3, 2]).
cell(cell, [2, 2]).
cell(visited, [2, 2]).
cell(cell, [1, 2]).
cell(visited, [1, 2]).
cell(cell, [0, 2]).
cell(wall, [0, 2]).
cell(cell, [1, 3]).
cell(visited, [1, 3]).
cell(cell, [2, 3]).
cell(breezeyes, [2, 3]).
cell(visited, [2, 3]).
cell(cell, [3, 3]).
cell(visited, [3, 3]).
cell(cell, [4, 3]).
cell(visited, [4, 3]).
cell(cell, [5, 3]).
cell(visited, [5, 3]).
cell(cell, [6, 3]).
cell(visited, [6, 3]).
cell(human0, [7, 3]).
cell(cell, [7, 3]).
cell(stenchyes, [7, 3]).
cell(visited, [7, 3]).
cell(wumpusdead, [7, 2]).
