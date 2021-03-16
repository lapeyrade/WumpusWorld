:-include('knowledgeBase.pl').

% DYNAMIC PART OF THE KNOWLEDGE BASE

wumpusTotal(1).
wumpusKilled(0).
arrowTotal(1).
arrowUsed(0).
goldTotal(1).
goldAgent(1).
cell(1,1,start).
cell(1,1,visited).
cell(1,0,safe).
cell(2,1,visited).
cell(2,1,breeze).
cell(0,1,wall).
cell(0,1,safe).
cell(1,2,visited).
cell(2,2,visited).
cell(2,2,stench).
cell(0,2,wall).
cell(0,2,safe).
cell(1,3,visited).
cell(0,3,safe).
cell(1,4,safe).
cell(2,3,visited).
cell(2,3,safe).
cell(3,3,safe).
cell(2,4,safe).
cell(3,2,safe).
cell(3,2,wumpusDead).
cell(1,3,safe).
cell(1,2,safe).
cell(1,1,agent).
cell(1,1,safe).
