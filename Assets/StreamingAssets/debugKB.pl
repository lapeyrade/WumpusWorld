:-include('KB.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_wumpus(1).
nb_wumpus_dead(0).
nb_arrow(1).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
% ------------------
% [1, 1], 
cell(1,1,start).
cell(1,1,visited).
cell(1,1,stenchno).
cell(1,1,wumpusno).
cell(1,0,emptyCell).
cell(1,0,wall).
cell(2,1,safe).
cell(2,1,visited).
cell(2,1,stenchno).
cell(2,1,wumpusno).
cell(2,0,emptyCell).
cell(2,0,wall).
cell(3,1,safe).
cell(3,1,visited).
cell(3,1,stenchno).
cell(3,1,wumpusno).
cell(1,1,safe).
cell(0,1,emptyCell).
cell(0,1,wall).
cell(1,1,agent).
