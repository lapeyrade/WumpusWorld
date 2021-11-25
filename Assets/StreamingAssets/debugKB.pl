:-include('KB.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

grid_coord(0,0,22,22).
nb_wumpus(10).
nb_wumpus_dead(0).
nb_arrow(10).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
personality(stochastic).
personality(hunter).
% ------------------
% [1, 1], 
cell(1,1,cell).
cell(1,1,start).
cell(1,1,agent).
cell(1,1,visited).
