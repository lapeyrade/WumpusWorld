:- include('kb.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_arrow(human, 5).
nb_gold(human, 0).
intelligence(human, 3).
strength(human, 7).
dexterity(human, 7).
personality(human, determinist).
personality(human, hunter).
personality(human, greedy).
% ------------------
cell(1, 1, cell).
cell(1, 1, start).
cell(1, 1, visited).
cell(2, 1, cell).
cell(2, 1, visited).
cell(3, 1, cell).
cell(3, 1, visited).
cell(4, 1, cell).
cell(4, 1, visited).
cell(5, 1, cell).
cell(5, 1, visited).
cell(6, 1, cell).
cell(6, 1, visited).
cell(7, 1, cell).
cell(7, 1, visited).
cell(8, 1, cell).
cell(8, 1, visited).
cell(9, 1, cell).
cell(9, 1, stenchyes).
cell(9, 1, breezeyes).
cell(9, 1, visited).
cell(8, 2, cell).
cell(8, 2, stenchyes).
cell(8, 2, visited).
cell(7, 2, cell).
cell(7, 2, visited).
cell(6, 2, cell).
cell(6, 2, visited).
cell(5, 2, cell).
cell(5, 2, visited).
cell(4, 2, cell).
cell(4, 2, visited).
cell(3, 2, cell).
cell(3, 2, breezeyes).
cell(3, 2, visited).
cell(2, 2, cell).
cell(2, 2, visited).
cell(1, 2, cell).
cell(1, 2, visited).
cell(0, 2, cell).
cell(0, 2, wall).
cell(1, 3, cell).
cell(1, 3, breezeyes).
cell(1, 3, visited).
cell(2, 3, cell).
cell(2, 3, breezeyes).
cell(2, 3, visited).
cell(4, 3, cell).
cell(4, 3, breezeyes).
cell(4, 3, visited).
cell(5, 3, cell).
cell(5, 3, stenchyes).
cell(5, 3, visited).
cell(5, 4, wumpusdead).
cell(6, 3, cell).
cell(6, 3, visited).
cell(7, 3, cell).
cell(7, 3, visited).
cell(8, 3, cell).
cell(8, 3, stenchyes).
cell(8, 3, visited).
cell(7, 4, cell).
cell(7, 4, visited).
cell(8, 4, cell).
cell(8, 4, visited).
cell(9, 4, cell).
cell(9, 4, stenchyes).
cell(9, 4, human).
cell(9, 4, visited).
cell(9, 3, wumpusdead).
cell(9, 2, wumpusdead).
