% :-include('knowledgeBase.pl').
:-include('declarative_kb2.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

nb_wumpus(1).
nb_wumpus_dead(0).
nb_arrow(1).
nb_arrow_used(0).
nb_gold(1).
nb_gold_agent(0).
% ------------------
cell(3,3,safe).
cell(3,3,stenchyes).
cell(3,3,visited).
cell(3,3,breeze).
cell(3,3,wumpusno).
cell(3,2,safe).
cell(3,2,stenchno).
cell(3,2,visited).
cell(3,2,wumpusno).
cell(3,1,safe).
cell(2,2,safe).
cell(2,2,stenchyes).
cell(2,2,visited).
cell(2,2,wumpusno).
cell(2,1,safe).
cell(2,1,agent).
cell(2,1,stenchno).
cell(2,1,visited).
cell(2,1,wumpusno).
cell(2,0,safe).
cell(1,3,safe).
cell(1,3,stenchyes).
cell(1,3,visited).
cell(1,3,wumpusno).
cell(1,2,safe).
cell(1,2,stenchno).
cell(1,2,visited).
cell(1,2,wumpusno).
cell(1,1,safe).
cell(1,1,stenchno).
cell(1,1,visited).
cell(1,1,wumpusno).
cell(1,1,start).
cell(1,0,safe).
cell(0,2,safe).
cell(0,1,safe).
cell(4,2,safe).
