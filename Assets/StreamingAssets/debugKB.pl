:-include('KB.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_wumpus(1).
nb_wumpus_dead(0).
nb_arrow(1).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
% ------------------
% [6, 1], [5, 1], [4, 1], [3, 1], [2, 1], [1, 1], 
cell(3,2,safe).
cell(3,1,safe).
cell(3,1,visited).
cell(3,1,stenchno).
cell(3,1,wumpusno).
cell(3,0,safe).
cell(2,2,safe).
cell(2,1,safe).
cell(2,1,visited).
cell(2,1,stenchno).
cell(2,1,wumpusno).
cell(2,0,safe).
cell(1,2,safe).
cell(1,1,safe).
cell(1,1,visited).
cell(1,1,start).
cell(1,1,stenchno).
cell(1,1,wumpusno).
cell(1,0,safe).
cell(0,1,safe).
cell(7,1,safe).
cell(6,2,safe).
cell(6,1,safe).
cell(6,1,agent).
cell(6,1,visited).
cell(6,1,stenchno).
cell(6,1,wumpusno).
cell(6,0,safe).
cell(5,2,safe).
cell(5,1,safe).
cell(5,1,visited).
cell(5,1,stenchno).
cell(5,1,wumpusno).
cell(5,0,safe).
cell(4,2,safe).
cell(4,1,safe).
cell(4,1,visited).
cell(4,1,stenchno).
cell(4,1,wumpusno).
cell(4,0,safe).
