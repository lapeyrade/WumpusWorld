:-include('KB.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

grid_coord(0,0,22,22).
nb_wumpus(2).
nb_wumpus_dead(0).
nb_arrow(2).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
% ------------------
% [1, 1], 
cell(1,1,start).
cell(1,1,agent).
cell(1,1,visited).
cell(1,1,stenchno).
cell(1,1,wumpusno).
cell(1,1,breezeno).
cell(1,1,pitno).
