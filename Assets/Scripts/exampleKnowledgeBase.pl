:- discontiguous([cell/3,
                move/2,
                moveAgent/2,
                writeKB/3]).

:- dynamic([cell/3,
            wumpusLeft/1,
            arrowLeft/1,
            goldLeft/1,
            goldAgent/1]).

resetKB():-
    retractall(cell(_, _, _)),
    retractall(wumpusTotal(_)),
    retractall(wumpusKilled(_)),
    retractall(goldTotal(_)),
    retractall(goldAgent(_)),
    retractall(arrowTotal(_)),
    retractall(arrowUsed(_)).

writeKB(X, Y, Name) :-
    append('Assets/Scripts/knowledgeBase.pl'),
    write("cell("),
    write(X),
    write(", "),
    write(Y),
    write(", "),
    write(Name),
    write(")."),
    nl,
    told.

isSafe(X, Y):-
    cell(X, Y, safe);
    cell(X, Y, wall);
    cell(X, Y, start);
    cell(X, Y, breeze);
    cell(X, Y, stench);
    cell(X, Y, visited);
    cell(X, Y, gold).

%%%%% DETECT PIT %%%%%
safeOrPit(X, Y):-
    isSafe(X, Y);
    cell(X, Y, pit).

% Up
findPit(X, Y):-
    A1 is Y-1, safeOrPit(X, A1),
    A2 is X+1, safeOrPit(A2, Y),
    A3 is X-1, safeOrPit(A3, Y),
    A4 is Y+1, assert(cell(X, A4, pit)),
    writeKB(X, A4, pit).

% Down
findPit(X, Y):-
    A1 is Y+1, safeOrPit(X, A1),
    A2 is X+1, safeOrPit(A2, Y),
    A3 is X-1, safeOrPit(A3, Y),
    A4 is Y-1, assert(cell(X, A4, pit)),
    writeKB(X, A4, pit).

% Left
findPit(X, Y):-
    A1 is Y-1, safeOrPit(X, A1),
    A2 is X+1, safeOrPit(A2, Y),
    A3 is Y+1, safeOrPit(X, A3),
    A4 is X-1, assert(cell(A4, Y, pit)),
    writeKB(X, A4, pit).

% Right
findPit(X, Y):-
    A1 is Y-1, safeOrPit(X, A1),
    A2 is X-1, safeOrPit(A2, Y),
    A3 is Y+1, safeOrPit(X, A3),
    A4 is X+1, assert(cell(A4, Y, pit)),
    writeKB(X, A4, pit).

%%%%% DETECT WUMPUS %%%%%
isWumpusDead(X, Y):-
    not(cell(X, Y, wumpus)).

notWumpus(X, Y):-
    isSafe(X, Y);
    cell(X, Y, wumpus).

% Up
addWumpus(X, Y):-
    A1 is Y+1, cell(X, A1, stench),
    A2 is Y+2, notWumpus(X, A2),
    A3 is X+1, notWumpus(A3, Y),
    A4 is X-1, notWumpus(A4, Y),
    not(cell(X, Y, wumpusDead)),
    not(cell(X, Y, wumpus)), assert(cell(X, Y, wumpus)).

% Down
addWumpus(X, Y):-
    A1 is Y-1, cell(X, A1, stench),
    A2 is Y-2, notWumpus(X, A2),
    A3 is X+1, notWumpus(A3, Y),
    A4 is X-1, notWumpus(A4, Y),
    not(cell(X, Y, wumpusDead)),
    not(cell(X, Y, wumpus)), assert(cell(X, Y, wumpus)).

% Left
addWumpus(X, Y):-
    A1 is X-1, cell(A1, Y, stench),
    A2 is X-2, notWumpus(A2, Y),
    A3 is Y+1, notWumpus(X, A3),
    A4 is Y-1, notWumpus(X, A4),
    not(cell(X, Y, wumpusDead)),
    not(cell(X, Y, wumpus)), assert(cell(X, Y, wumpus)).

% Right
addWumpus(X, Y):-
    A1 is X+1, cell(A1, Y, stench),
    A2 is X+2, notWumpus(A2, Y),
    A3 is Y+1, notWumpus(X, A3),
    A4 is Y-1, notWumpus(X, A4),
    not(cell(X, Y, wumpusDead)),
    not(cell(X, Y, wumpus)), assert(cell(X, Y, wumpus)).

killWumpus(X, Y):-
    arrowLeft(Arrow), Arrow >= 1, NewarrowLeft is (Arrow - 1),
    retract(arrowLeft(Arrow)), assert(arrowLeft(NewarrowLeft)),
    assert(cell(X, Y, wumpusDead)),
    retract(cell(X, Y, wumpus)).

%%%%%%%%%% AGENT MOVEMENT %%%%%%%%%%

%%%%% CELL BREEZE %%%%%
moveAgent(X, Y):-
    cell(X, Y, breeze),
    findPit(X, Y).

%%%%% CELL SAFE %%%%%
moveAgent(X, Y):-
    (not(cell(Y, X, safe)) -> assert(cell(X, Y, safe))),
    (not(cell(Y, X, visited)) -> assert(cell(X, Y, visited))),
    A1 is X+1, cellSafe(A1, Y);
    A2 is X-1, cellSafe(A2, Y);
    A3 is Y+1, cellSafe(X, A3);
    A4 is Y-1, cellSafe(X, A4);
    move(X, Y).
    

cellSafe(X, Y):-
    not(cell(X, Y, wall)),
    not(cell(X, Y, safe)),
    assert(cell(X, Y, safe)).

% Move Right
move(X, Y):-
    A1 is X+1, not(cell(A1, Y, wall)),
    A1 is X+1, assert(cell(A1, Y, agent)),
    retract(cell(X, Y, agent)).

% Move Left
move(X, Y):-
    A1 is X-1, not(cell(A1, Y, wall)),
    A1 is X-1, assert(cell(A1, Y, agent)),
    retract(cell(X, Y, agent)).

% Move Down
move(X, Y):-
    A1 is Y+1, not(cell(X, A2, wall)),
    A1 is Y+1, assert(cell(X, A2, agent)),
    retract(cell(X, Y, agent)).

% Move Up
move(X, Y):-
    A1 is Y-1, not(cell(X, A2, wall)),
    A1 is Y-1, assert(cell(X, A2, agent)),
    retract(cell(X, Y, agent)).

surroundingSafe(X, Y):-
    A1 is X+1, markSafe(A1, Y);
    A2 is X-1, markSafe(A2, Y);
    A3 is Y+1, markSafe(X, A3);
    A4 is Y-1, markSafe(X, A4).
    
markSafe(X, Y):-
    not(cell(X, Y, _)),
    assert(cell(X, Y, safe)),
    writeKB(X, Y, safe).

nextMove(X, Y):-
    cell(X, Y, safe),
    surroundingSafe(X, Y).

% Everything after the following line will be deleted at start
% DYNAMIC PART OF THE KNOWLEDGE BASE