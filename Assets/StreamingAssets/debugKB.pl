:-include('KB.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

grid_coord(0,0,12,12).
nb_wumpus(5).
nb_wumpus_dead(0).
nb_arrow(5).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
personality(stochastic).
% ------------------
% [1, 1], 
cell(1,1,cell).
cell(1,1,start).
cell(1,1,agent).
cell(1,1,visited).
