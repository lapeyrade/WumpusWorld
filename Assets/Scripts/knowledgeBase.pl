:- use_module(library(random)).

:- dynamic([cell/3, allGoldsFound/1]).

% Erase all knowledges
resetKB():-
    retractall(cell(_, _, _)),
    retractall(wumpusTotal(_)),
    retractall(wumpusKilled(_)),
    retractall(goldTotal(_)),
    retractall(goldAgent(_)),
    retractall(arrowTotal(_)),
    retractall(arrowUsed(_)),
    retractall(allGoldsFound(_)).

% Initiate attributes values
initGameAttributes(Wumpuses, Golds):-
    assertz(wumpusTotal(Wumpuses)),
    assertz(wumpusKilled(0)),
    assertz(arrowTotal(Wumpuses)),
    assertz(arrowUsed(0)),
    assertz(goldTotal(Golds)),
    assertz(goldAgent(0)),
    assertz(allGoldsFound(false)).

% Initiate agent attributes
initAgent(X, Y):-
    assertz(cell(X, Y, start)),
    assertz(cell(X, Y, agent)),
    assertz(cell(X, Y, visited)),
    surroundingSafe(X, Y).

% Check if side cells are safe
surroundingSafe(X, Y):-
    X2 is X+1,  markSafe(X2, Y),
    X3 is X-1,  markSafe(X3, Y),
    Y2 is Y+1,  markSafe(X, Y2),
    Y3 is Y-1,  markSafe(X, Y3).

% If cell safe mark it safe 
markSafe(X, Y):-
    (not(cell(X, Y, safe)),
    not(cell(X, Y, visited))) ->
    assertz(cell(X, Y, safe)); true.

% Check is cell is safe or start
safeOrStart(X, Y):-
    cell(X, Y, start);
    cell(X, Y, safe).

% Check is cell is safe or visited and without dead wumpus
safeOrVisited(X, Y):-
    \+(cell(X, Y, wumpusDead)),
    (cell(X, Y, visited);
    cell(X, Y, safe)).

%%%%%%%%%% AGENT MOVEMENT %%%%%%%%%%

%%%% DETERMINISTIC MOVEMENT %%%%
% Check side cells
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    checkNearCells(X1, Y1),
    nextMove(X1, Y1, X2, Y2, X3, Y3).

% Go back to start cell
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    allGoldsFound(true),
    X2 is X3, Y2 is Y3.

% If right cell contains dead wumpus & not visited go right
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, stench),
    X2 is X1+1, Y2 is Y1, % Right
    cell(X2, Y2, wumpusDead),
    \+cell(X2, Y2, visited).

% If left cell contains dead wumpus & not visited go left
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, stench),
    X2 is X1-1, Y2 is Y1, % left
    cell(X2, Y2, wumpusDead),
    \+cell(X2, Y2, visited).

% If up cell contains dead wumpus & not visited go up
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, stench),
    Y2 is Y1+1, X2 is X1, % Up
    cell(X2, Y2, wumpusDead),
    \+cell(X2, Y2, visited).

% If down cell contains dead wumpus & not visited go down
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, stench),
    Y2 is Y1-1, X2 is X1, % Down
    cell(X2, Y2, wumpusDead),
    \+cell(X2, Y2, visited).

% If right cell not visited and not wall go right
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    safeOrStart(X1, Y1),
    X2 is X1+1, Y2 is Y1, % Right
    \+(cell(X2, Y2, wall)),
    \+(cell(X2, Y2, visited)).

% If left cell not visited and not wall go left
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    safeOrStart(X1, Y1),
    X2 is X1-1, Y2 is Y1, % Left
    \+(cell(X2, Y2, wall)),
    \+(cell(X2, Y2, visited)).

% If up cell not visited and not wall go up
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    safeOrStart(X1, Y1),
    Y2 is Y1+1, X2 is X1, % Up
    \+(cell(X2, Y2, wall)),
    \+(cell(X2, Y2, visited)).

% If down cell not visited and not wall go down
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    safeOrStart(X1, Y1),
    Y2 is Y1-1, X2 is X1, % Down
    \+(cell(X2, Y2, wall)),
    \+(cell(X2, Y2, visited)).

% Otherwise go to last cell
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    X2 is X3, Y2 is Y3.

%%%% RANDOM MOVEMENT %%%%
% Move randomly to a side cell
randomMove(X1, Y1, X2, Y2):-
    random_between(1, 4, Random),
    move(X1, Y1, X2, Y2, Random).

% Move right
move(X1, Y1, X2, Y2, 1):- % Right
    X2 is X1+1, Y2 is Y1. 

% Move left
move(X1, Y1, X2, Y2, 2):- % Left
    X2 is X1-1, Y2 is Y1.

% Move down
move(X1, Y1, X2, Y2, 3):- % Down
    Y2 is Y1-1, X2 is X1.

% Move up
move(X1, Y1, X2, Y2, 4):- % Up
    Y2 is Y1+1, X2 is X1.

%%%%%%%%%% NEAR CELL DETECTION %%%%%%%%%%
% Check if side cells are safe
checkNearCells(X, Y):-
    cell(X, Y, safe),
    surroundingSafe(X, Y),
    false.

% if cell has stench check for wumpus
checkNearCells(X, Y):-
    cell(X, Y, stench),
    findElement(X, Y, wumpus),
    false.

% if cell has breeze check for pit
checkNearCells(X, Y):-
    cell(X, Y, breeze),
    findElement(X, Y, pit),
    false.

% if right cell has stench check for wumpus
checkNearCells(X, Y):-
    X2 is X+1, cell(X2, Y, stench), % Right
    findElement(X2, Y, wumpus),
    false.

% if left cell has stench check for wumpus
checkNearCells(X, Y):-
    X2 is X-1, cell(X2, Y, stench), % Left
    findElement(X2, Y, wumpus).

% if up cell has stench check for wumpus
checkNearCells(X, Y):-
    Y2 is Y+1, cell(X, Y2, stench), % Up
    findElement(X, Y2, wumpus),
    false.

% if down cell has stench check for wumpus
checkNearCells(X, Y):-
    Y2 is Y-1, cell(X, Y2, stench), % Down
    findElement(X, Y2, wumpus),
    false.

% if right cell has breeze check for pit
checkNearCells(X, Y):-
    X2 is X+1, cell(X2, Y, breeze), % Right
    findElement(X2, Y, pit),
    false.

% if left cell has breeze check for pit
checkNearCells(X, Y):-
    X2 is X-1, cell(X2, Y, breeze), % Left
    findElement(X2, Y, pit),
    false.

% if up cell has breeze check for pit
checkNearCells(X, Y):-
    Y2 is Y+1, cell(X, Y2, breeze), % Up
    findElement(X, Y2, pit),
    false.

% if down cell has breeze check for pit
checkNearCells(X, Y):-
    Y2 is Y-1, cell(X, Y2, breeze), % Down
    findElement(X, Y2, pit),
    false.

% Deduction on what is in right cell
findElement(X, Y, Z):-
    A1 is Y-1, safeOrVisited(X, A1),
    A2 is X-1, safeOrVisited(A2, Y),
    A3 is Y+1, safeOrVisited(X, A3),
    A4 is X+1, \+(cell(A4, Y, wumpusDead)), % Right
    \+(cell(X, A4, Z)), assertz(cell(A4, Y, Z)).

% Deduction on what is in left cell
findElement(X, Y, Z):-
    A1 is Y-1, safeOrVisited(X, A1),
    A2 is X+1, safeOrVisited(A2, Y),
    A3 is Y+1, safeOrVisited(X, A3),
    A4 is X-1, \+(cell(A4, Y, wumpusDead)), % Left
    \+(cell(X, A4, Z)), assertz(cell(A4, Y, Z)).

% Deduction on what is in up cell
findElement(X, Y, Z):-
    A1 is Y-1, safeOrVisited(X, A1),
    A2 is X+1, safeOrVisited(A2, Y),
    A3 is X-1, safeOrVisited(A3, Y),
    A4 is Y+1, \+(cell(X, A4, wumpusDead)), % Up
    \+(cell(X, A4, Z)), assertz(cell(X, A4, Z)).

% Deduction on what is in down cell
findElement(X, Y, Z):-
    A1 is Y+1, safeOrVisited(X, A1),
    A2 is X+1, safeOrVisited(A2, Y),
    A3 is X-1, safeOrVisited(A3, Y),
    A4 is Y-1, \+(cell(X, A4, wumpusDead)), % Down
    \+(cell(X, A4, Z)), assertz(cell(X, A4, Z)).