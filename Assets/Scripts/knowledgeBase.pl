:- use_module(library(random)).

:- dynamic([cell/3, allGoldsFound/1]).

%%%%%%%%%% KNOWLEDGEBASE MANAGEMENT %%%%%%%%%%
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
    assertz(allGoldsFound()).

% Initiate agent attributes
initAgent(X, Y):-
    assertz(cell(X, Y, start)),
    assertz(cell(X, Y, agent)),
    assertz(cell(X, Y, visited)),
    nb_setval(stackX, [X]),
    nb_setval(stackY, [Y]),
    surroundingSafe(X, Y).

%%%%%%%%%% STACK MANAGEMENT %%%%%%%%%%
% Add Element at the 1st place of stack
push(Element, Stack, [Element|Stack]).
% Remove 1st element from stack
pop(Element, [Element|Stack], Stack).

% Add Coordinate to stack
pushStack(NewElement, Coord):-
    nb_getval(Coord, Stack),
    push(NewElement, Stack, NewStack),
    nb_setval(Coord, NewStack).

% Show 1st Element from stack
peekStack(TopElement, Coord):-
    nb_getval(Coord, Stack),
    push(TopElement, _, Stack).

% Remove 1st Element from stack
popStack(Coord):-
    nb_getval(Coord, Stack),
    pop(_, Stack, NewStack),
    nb_setval(Coord, NewStack).

%%%%%%%%%% AGENT MOVEMENT %%%%%%%%%%

%%%% DETERMINISTIC MOVEMENT %%%%

% Go back to start cell
nextMove(_, _, NewX, NewY):-
    allGoldsFound(true),
    popStack(stackX), popStack(stackY),
    peekStack(PrevX, stackX), peekStack(PrevY, stackY),
    NewX is PrevX, NewY is PrevY.

% If right cell contains dead wumpus & not visited go right
nextMove(X, Y, NewX, NewY):-
    cell(X, Y, stench),
    NewX is X+1, NewY is Y, % Right
    cell(NewX, NewY, wumpusDead),
    \+cell(NewX, NewY, visited),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If left cell contains dead wumpus & not visited go left
nextMove(X, Y, NewX, NewY):-
    cell(X, Y, stench),
    NewX is X-1, NewY is Y, % left
    cell(NewX, NewY, wumpusDead),
    \+cell(NewX, NewY, visited),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If up cell contains dead wumpus & not visited go up
nextMove(X, Y, NewX, NewY):-
    cell(X, Y, stench),
    NewY is Y+1, NewX is X, % Up
    cell(NewX, NewY, wumpusDead),
    \+cell(NewX, NewY, visited),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If down cell contains dead wumpus & not visited go down
nextMove(X, Y, NewX, NewY):-
    cell(X, Y, stench),
    NewY is Y-1, NewX is X, % Down
    cell(NewX, NewY, wumpusDead),
    \+cell(NewX, NewY, visited),
    pushStack(NewX, stackX), pushStack(NewY, stackY).


% If right cell not visited and not wall go right
nextMove(X, Y, NewX, NewY):-
    safeOrStart(X, Y),
    NewX is X+1, NewY is Y, % Right
    \+(cell(NewX, NewY, wall)),
    \+(cell(NewX, NewY, visited)),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If left cell not visited and not wall go left
nextMove(X, Y, NewX, NewY):-
    safeOrStart(X, Y),
    NewX is X-1, NewY is Y, % Left
    \+(cell(NewX, NewY, wall)),
    \+(cell(NewX, NewY, visited)),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If up cell not visited and not wall go up
nextMove(X, Y, NewX, NewY):-
    safeOrStart(X, Y),
    NewY is Y+1, NewX is X, % Up
    \+(cell(NewX, NewY, wall)),
    \+(cell(NewX, NewY, visited)),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% If down cell not visited and not wall go down
nextMove(X, Y, NewX, NewY):-
    safeOrStart(X, Y),
    NewY is Y-1, NewX is X, % Down
    \+(cell(NewX, NewY, wall)),
    \+(cell(NewX, NewY, visited)),
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% Otherwise go to previous cell
nextMove(_, _, NewX, NewY):-
    popStack(stackX), popStack(stackY),
    peekStack(PrevX, stackX), peekStack(PrevY, stackY),
    NewX is PrevX, NewY is PrevY.

%%%% RANDOM MOVEMENT %%%%
% Move randomly to a side cell
randomMove(X, Y, NewX, NewY):-
    random_between(1, 4, Random),
    move(X, Y, NewX, NewY, Random).

% Move right
move(X, Y, NewX, NewY, 1):- % Right
    NewX is X+1, NewY is Y,
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% Move left
move(X, Y, NewX, NewY, 2):- % Left
    NewX is X-1, NewY is Y,
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% Move down
move(X, Y, NewX, NewY, 3):- % Down
    NewY is Y-1, NewX is X,
    pushStack(NewX, stackX), pushStack(NewY, stackY).

% Move up
move(X, Y, NewX, NewY, 4):- % Up
    NewY is Y+1, NewX is X,
    pushStack(NewX, stackX), pushStack(NewY, stackY).

%%%%%%%%%% NEAR CELL DETECTION %%%%%%%%%%
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

% Check if side cells are safe
checkNearCells(X, Y):-
    cell(X, Y, safe),
    surroundingSafe(X, Y), fail.

% if cell has stench check for wumpus
checkNearCells(X, Y):-
    cell(X, Y, stench),
    findElement(X, Y, wumpus), fail.

% if cell has breeze check for pit
checkNearCells(X, Y):-
    cell(X, Y, breeze),
    findElement(X, Y, pit), fail.

% if right cell has stench check for wumpus
checkNearCells(X, Y):-
    NewX is X+1, cell(NewX, Y, stench), % Right
    findElement(NewX, Y, wumpus), fail.

% if left cell has stench check for wumpus
checkNearCells(X, Y):-
    NewX is X-1, cell(NewX, Y, stench), % Left
    findElement(NewX, Y, wumpus).

% if up cell has stench check for wumpus
checkNearCells(X, Y):-
    NewY is Y+1, cell(X, NewY, stench), % Up
    findElement(X, NewY, wumpus), fail.

% if down cell has stench check for wumpus
checkNearCells(X, Y):-
    NewY is Y-1, cell(X, NewY, stench), % Down
    findElement(X, NewY, wumpus), fail.

% if right cell has breeze check for pit
checkNearCells(X, Y):-
    NewX is X+1, cell(NewX, Y, breeze), % Right
    findElement(NewX, Y, pit), fail.

% if left cell has breeze check for pit
checkNearCells(X, Y):-
    NewX is X-1, cell(NewX, Y, breeze), % Left
    findElement(NewX, Y, pit), fail.

% if up cell has breeze check for pit
checkNearCells(X, Y):-
    NewY is Y+1, cell(X, NewY, breeze), % Up
    findElement(X, NewY, pit), fail.

% if down cell has breeze check for pit
checkNearCells(X, Y):-
    NewY is Y-1, cell(X, NewY, breeze), % Down
    findElement(X, NewY, pit), fail.

% Deduction on what is in right cell
findElement(X, Y, Z):-
    Y2 is Y-1, safeOrVisited(X, Y2),
    Y3 is Y+1, safeOrVisited(X, Y3),
    X2 is X-1, safeOrVisited(X2, Y),
    X3 is X+1, \+(cell(X3, Y, wumpusDead)), % Right
    \+(cell(X3, Y, Z)), assertz(cell(X3, Y, Z)).

% Deduction on what is in left cell
findElement(X, Y, Z):-
    Y2 is Y-1, safeOrVisited(X, Y2),
    Y3 is Y+1, safeOrVisited(X, Y3),
    X2 is X+1, safeOrVisited(X2, Y),
    X3 is X-1, \+(cell(X3, Y, wumpusDead)), % Left
    \+(cell(X3, Y, Z)), assertz(cell(X3, Y, Z)).

% Deduction on what is in up cell
findElement(X, Y, Z):-
    X2 is X+1, safeOrVisited(X2, Y),
    X3 is X-1, safeOrVisited(X3, Y),
    Y2 is Y-1, safeOrVisited(X, Y2),
    Y3 is Y+1, \+(cell(X, Y3, wumpusDead)), % Up
    \+(cell(X, Y3, Z)), assertz(cell(X, Y3, Z)).

% Deduction on what is in down cell
findElement(X, Y, Z):-
    X2 is X+1, safeOrVisited(X2, Y),
    X3 is X-1, safeOrVisited(X3, Y),
    Y2 is Y+1, safeOrVisited(X, Y2),
    Y3 is Y-1, \+(cell(X, Y3, wumpusDead)), % Down
    \+(cell(X, Y3, Z)), assertz(cell(X, Y3, Z)).