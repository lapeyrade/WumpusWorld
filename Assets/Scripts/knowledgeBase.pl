:- use_module(library(random)).

:- dynamic([cell/3, allGoldsFound/1]).

resetKB():-
    retractall(cell(_, _, _)),
    retractall(wumpusTotal(_)),
    retractall(wumpusKilled(_)),
    retractall(goldTotal(_)),
    retractall(goldAgent(_)),
    retractall(arrowTotal(_)),
    retractall(arrowUsed(_)),
    retractall(allGoldsFound(_)).


initGameAttributes(Wumpuses, Golds):-
    assertz(wumpusTotal(Wumpuses)),
    assertz(wumpusKilled(0)),
    assertz(arrowTotal(Wumpuses)),
    assertz(arrowUsed(0)),
    assertz(goldTotal(Golds)),
    assertz(goldAgent(0)),
    assertz(allGoldsFound(false)).

initAgent(X, Y):-
    assertz(cell(X, Y, start)),
    assertz(cell(X, Y, agent)),
    assertz(cell(X, Y, visited)),
    surroundingSafe(X, Y).

surroundingSafe(X, Y):-
    X2 is X+1,  markSafe(X2, Y),
    X3 is X-1,  markSafe(X3, Y),
    Y2 is Y+1,  markSafe(X, Y2),
    Y3 is Y-1,  markSafe(X, Y3).

markSafe(X, Y):-
    (not(cell(X, Y, safe)),
    not(cell(X, Y, visited))) ->
    assertz(cell(X, Y, safe)); true.

%%%%%%%%%% AGENT MOVEMENT %%%%%%%%%%

%%%% DETERMINISTIC MOVEMENT %%%%
nextMove(X1, Y1, X2, Y2, X3, Y3):-
    checkNearCells(X1, Y1).

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    allGoldsFound(true),
    X2 is X3, Y2 is Y3.

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, safe),
    X2 is X1+1, Y2 is Y1, % Right
    \+(cell(X2, Y2, visited)).

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, safe),
    X2 is X1-1, Y2 is Y1, % Left
    \+(cell(X2, Y2, visited)).

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, safe),
    Y2 is Y1+1, X2 is X1, % Down
    \+(cell(X2, Y2, visited)).

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    cell(X1, Y1, safe),
    Y2 is Y1-1, X2 is X1, % Up
    \+(cell(X2, Y2, visited)).

nextMove(X1, Y1, X2, Y2, X3, Y3):-
    X2 is X3, Y2 is Y3.


%%%% RANDOM MOVEMENT %%%%
randomMove(X1, Y1, X2, Y2):-
    random_between(1, 4, Random),
    move(X1, Y1, X2, Y2, Random).

move(X1, Y1, X2, Y2, 1):- % Right
    X2 is X1+1, Y2 is Y1. 

move(X1, Y1, X2, Y2, 2):- % Left
    X2 is X1-1, Y2 is Y1.

move(X1, Y1, X2, Y2, 3):- % Down
    Y2 is Y1+1, X2 is X1.

move(X1, Y1, X2, Y2, 4):- % Up
    Y2 is Y1-1, X2 is X1.

isSafe(X, Y):-
    cell(X, Y, visited);
    cell(X, Y, safe).

checkNearCells(X, Y):-
    cell(X, Y, safe),
    surroundingSafe(X, Y).

%%%%%%%%%% PIT & WUMPUS DETECTION %%%%%%%%%%

checkNearCells(X, Y):-
    X2 is X+1, cell(X2, Y, stench),
    findElement(X2, Y, wumpus).

checkNearCells(X, Y):-
    X2 is X-1, cell(X2, Y, stench),
    findElement(X2, Y, wumpus).

checkNearCells(X, Y):-
    Y2 is Y+1, cell(X, Y2, stench),
    findElement(X, Y2, wumpus).

checkNearCells(X, Y):-
    Y2 is Y-1, cell(X, Y2, stench),
    findElement(X, Y2, wumpus).

checkNearCells(X, Y):-
    X2 is X+1, cell(X2, Y, breeze),
    findElement(X2, Y, pit).

checkNearCells(X, Y):-
    X2 is X-1, cell(X2, Y, breeze),
    findElement(X2, Y, pit).

checkNearCells(X, Y):-
    Y2 is Y+1, cell(X, Y2, breeze),
    findElement(X, Y2, pit).

checkNearCells(X, Y):-
    Y2 is Y-1, cell(X, Y2, breeze),
    findElement(X, Y2, pit).

checkNearCells(X, Y):-
    cell(X, Y2, stench),
    findElement(X, Y, wumpus).

checkNearCells(X, Y):-
    cell(X, Y2, breeze),
    findElement(X, Y, pit).

% Up
findElement(X, Y, Z):-
    A1 is Y-1, isSafe(X, A1),
    A2 is X+1, isSafe(A2, Y),
    A3 is X-1, isSafe(A3, Y),
    A4 is Y+1, not(cell(X, Y, Z)),
    not(cell(X, A4, safe)),
    not(cell(A4, Y, wumpusDead)),
    assertz(cell(X, A4, Z)).

% Down
findElement(X, Y, Z):-
    A1 is Y+1, isSafe(X, A1),
    A2 is X+1, isSafe(A2, Y),
    A3 is X-1, isSafe(A3, Y),
    A4 is Y-1, not(cell(X, Y, Z)),
    not(cell(X, A4, safe)),
    not(cell(A4, Y, wumpusDead)),
    assertz(cell(X, A4, Z)).

% Left
findElement(X, Y, Z):-
    A1 is Y-1, isSafe(X, A1),
    A2 is X+1, isSafe(A2, Y),
    A3 is Y+1, isSafe(X, A3),
    A4 is X-1, not(cell(X, Y, Z)),
    not(cell(A4, Y, safe)),
    not(cell(A4, Y, wumpusDead)),
    assertz(cell(A4, Y, Z)).

% Right
findElement(X, Y, Z):-
    A1 is Y-1, isSafe(X, A1),
    A2 is X-1, isSafe(A2, Y),
    A3 is Y+1, isSafe(X, A3),
    A4 is X+1, not(cell(X, Y, Z)),
    not(cell(A4, Y, safe)),
    not(cell(A4, Y, wumpusDead)),
    assertz(cell(A4, Y, Z)).