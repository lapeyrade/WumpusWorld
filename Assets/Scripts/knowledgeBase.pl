:- use_module(library(random)).

:- dynamic([cell/3, allGoldsFound/1, world/3, nbWumpus/1, nbGold/1, nbArrow/1,
            nbWumpusDead/1, nbArrowUsed/1, allGoldsFound/1, nbGoldAgent/1]).

%%%%%%%%%% CUSTOM PREDICATES %%%%%%%%%%
% Check if fact exists, if not assert it
assertx(Fact):-
    \+(Fact) -> assertz(Fact); true.

%%%%%%%%%% KNOWLEDGEBASE MANAGEMENT %%%%%%%%%%
% Erase all knowledges
resetKB():-
    retractall(cell(_, _, _)),
    retractall(world(_, _, _)),
    retractall(nbWumpus(_)),
    retractall(nbWumpusDead(_)),
    retractall(nbArrow(_)),
    retractall(nbArrowUsed(_)),
    retractall(nbGold(_)),
    retractall(nbGoldAgent(_)),
    retractall(allGoldsFound(_)).

% Initiate the game with attributes  Ex: initGame(1, 1, 0, 0, 4, 4, 1, 1, 3).
initGame(RandSeed, StartCol, StartRow, MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NBGold, NBWumpus, NBPit):-
    set_random(seed(RandSeed)),
    initAgent(StartCol, StartRow),
    initGameAttributes(NBWumpus, NBGold),
    assertx(world(StartCol, StartRow, start)),
    initWallCol(MinGridCol, MinGridRow, MaxGridRow); % Left
    initWallCol(MaxGridCol, MinGridRow, MaxGridRow); % Right
    initWallRow(MinGridCol, MinGridRow, MaxGridCol); % Bot
    initWallRow(MinGridCol, MaxGridRow, MaxGridCol); % Top
    initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBGold);
    initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBWumpus);
    initPit(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBPit).

% Initiate attributes values
initGameAttributes(NBWumpus, NBGold):-
    assertx(nbWumpus(NBWumpus)),
    assertx(nbWumpusDead(0)),
    assertx(nbArrow(NBWumpus)),
    assertx(nbArrowUsed(0)),
    assertx(nbGold(NBGold)),
    assertx(nbGoldAgent(0)),
    assertx(allGoldsFound(false)).

% Initiate agent attributes
initAgent(Col, Row):-
    assertx(cell(Col, Row, start)),
    assertx(cell(Col, Row, agent)),
    assertx(cell(Col, Row, visited)),
    nb_setval(stackCol, [Col]),
    nb_setval(stackRow, [Row]),
    surroundingSafe(Col, Row).

% Initiate Top & Bottom Walls
initWallRow(Col, Row, MaxGridCol):-
    assertx(world(Col, Row, wall)); Col < MaxGridCol,
    NewCol is Col + 1, initWallRow(NewCol, Row, MaxGridCol).

% Initiate Left & Right Walls
initWallCol(Col, Row, MaxGridRow):-
    assertx(world(Col, Row, wall)); Row < MaxGridRow,
    NewRow is Row + 1, initWallCol(Col, NewRow, MaxGridRow).

% Initiate Golds Ex: initGold(0, 0, 4, 4, 0, 1).
initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Gold, NBGold):-
    Gold < NBGold,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertx(world(NewCol, NewRow, gold)),
    NewGold is Gold + 1); NewGold is Gold),
    initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NewGold, NBGold).

% Initiate Wumpus Ex: initWumpus(0, 0, 4, 4, 0, 1).
initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Wumpus, NBWumpus):-
    Wumpus < NBWumpus,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertx(world(NewCol, NewRow, wumpus)),
    NewWumpus is Wumpus + 1, generateAroundCell(NewCol, NewRow, stench)); NewWumpus is Wumpus),
    initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NewWumpus, NBWumpus).

% Initiate Pits Ex: initPit(0, 0, 4, 4, 0, 3).
initPit(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Pit, NBPit):-
    Pit < NBPit,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertx(world(NewCol, NewRow, pit)),
    NewPit is Pit + 1, generateAroundCell(NewCol, NewRow, breeze)); NewPit is Pit),
    initPit(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NewPit, NBPit).

% If cell is not alerady taken
cellFree(Col, Row):-
    \+(world(Col, Row, pit)),
    \+(world(Col, Row, wumpus)),
    \+(world(Col, Row, start)),
    \+(world(Col, Row, gold)),
    \+(world(Col, Row, wall)).

% Generate Freeze or Stench around a cell
generateAroundCell(Col, Row, State):-
    RightCol is Col+1,assertx(world(RightCol, Row, State)),
    LeftCol is Col-1, assertx(world(LeftCol, Row, State)),
    UpRow is Row+1, assertx(world(Col, UpRow, State)),
    DownRow is Row-1, assertx(world(Col, DownRow, State)).

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

% If cell safe & not already marked as safe, mark it
nextMove(Col, Row, NewCol, NewRow):-
    ((\+(cell(Col, Row, breeze)),
    \+(cell(Col, Row, stench)),
    \+(cell(Col, Row, wumpus)),
    \+(cell(Col, Row, pit)),
    \+(cell(Col, Row, start))) ->
    assertx(cell(Col, Row, safe)); true),
    chooseMove(Col, Row, NewCol, NewRow),
    checkNextCell(Col, Row, NewCol, NewRow).

% Go back to start cell
chooseMove(_, _, NewCol, NewRow):-
    allGoldsFound(true),
    popStack(stackCol), popStack(stackRow),
    peekStack(PrevCol, stackCol), peekStack(PrevRow, stackRow),
    NewCol is PrevCol, NewRow is PrevRow.

% If right cell contains dead wumpus & not visited go right
chooseMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewCol is Col+1, NewRow is Row, % Right
    cell(NewCol, NewRow, wumpusDead),
    \+(cell(NewCol, NewRow, visited)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If left cell contains dead wumpus & not visited go left
chooseMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewCol is Col-1, NewRow is Row, % left
    cell(NewCol, NewRow, wumpusDead),
    \+(cell(NewCol, NewRow, visited)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If up cell contains dead wumpus & not visited go up
chooseMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewRow is Row+1, NewCol is Col, % Up
    cell(NewCol, NewRow, wumpusDead),
    \+(cell(NewCol, NewRow, visited)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If down cell contains dead wumpus & not visited go down
chooseMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewRow is Row-1, NewCol is Col, % Down
    cell(NewCol, NewRow, wumpusDead),
    \+(cell(NewCol, NewRow, visited)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackCol).

% If right cell not visited and not wall go right
chooseMove(Col, Row, NewCol, NewRow):-
    safeOrStart(Col, Row),
    NewCol is Col+1, NewRow is Row, % Right
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If left cell not visited and not wall go left
chooseMove(Col, Row, NewCol, NewRow):-
    safeOrStart(Col, Row),
    NewCol is Col-1, NewRow is Row, % Left
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If up cell not visited and not wall go up
chooseMove(Col, Row, NewCol, NewRow):-
    safeOrStart(Col, Row),
    NewRow is Row+1, NewCol is Col, % Up
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% If down cell not visited and not wall go down
chooseMove(Col, Row, NewCol, NewRow):-
    safeOrStart(Col, Row),
    NewRow is Row-1, NewCol is Col, % Down
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% Otherwise go to previous cell
chooseMove(_, _, NewCol, NewRow):-
    popStack(stackCol), popStack(stackRow),
    peekStack(PrevCol, stackCol), peekStack(PrevRow, stackRow),
    NewCol is PrevCol, NewRow is PrevRow.

% Sense what is in destination cell
checkNextCell(Col, Row, NewCol, NewRow):-
    (world(NewCol, NewRow, wall) -> (assertx(cell(NewCol, NewRow, wall)),
     retract(cell(NewCol, NewRow, safe))); (retract(cell(Col, Row, agent)),
      assertx(cell(NewCol, NewRow, agent)), assertx(cell(NewCol, NewRow, visited)))),
    (world(NewCol, NewRow, breeze) -> (retract(cell(NewCol, NewRow, safe)), assertx(cell(NewCol, NewRow, breeze))); true),
    (world(NewCol, NewRow, stench) -> (retract(cell(NewCol, NewRow, safe)), assertx(cell(NewCol, NewRow, stench))); true),
    (world(NewCol, NewRow, gold) -> assertx(cell(NewCol, NewRow, gold)); true),
    (world(NewCol, NewRow, pit) -> assertx(cell(NewCol, NewRow, pit)); true),
    (world(NewCol, NewRow, wumpus) -> assertx(cell(NewCol, NewRow, wumpus)); true).
    
%%%% RANDOM MOVEMENT %%%%
% Move randomly to a side cell
randomMove(Col, Row, NewCol, NewRow):-
    random_between(1, 4, Random),
    move(Col, Row, NewCol, NewRow, Random).

% Move right
move(Col, Row, NewCol, NewRow, 1):- % Right
    NewCol is Col+1, NewRow is Row,
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% Move left
move(Col, Row, NewCol, NewRow, 2):- % Left
    NewCol is Col-1, NewRow is Row,
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% Move down
move(Col, Row, NewCol, NewRow, 3):- % Down
    NewRow is Row-1, NewCol is Col,
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

% Move up
move(Col, Row, NewCol, NewRow, 4):- % Up
    NewRow is Row+1, NewCol is Col,
    pushStack(NewCol, stackCol), pushStack(NewRow, stackRow).

%%%%%%%%%% NEAR CELL DETECTION %%%%%%%%%%
% Check if side cells are safe
surroundingSafe(Col, Row):-
    Col2 is Col+1,  markSafe(Col2, Row),
    Col3 is Col-1,  markSafe(Col3, Row),
    Row2 is Row+1,  markSafe(Col, Row2),
    Row3 is Row-1,  markSafe(Col, Row3).

% If cell not visited, mark it safe 
markSafe(Col, Row):-
    \+(cell(Col, Row, visited)) ->
    assertz(cell(Col, Row, safe)); true.

% Check is cell is safe or start
safeOrStart(Col, Row):-
    cell(Col, Row, start);
    cell(Col, Row, safe).

% Check is cell is safe or visited and without dead wumpus
safeOrVisited(Col, Row):-
    \+(cell(Col, Row, wumpusDead)),
    (cell(Col, Row, visited);
    cell(Col, Row, safe)).

% Check is cell is wall or visited
wallOrVisited(Col, Row):-
    cell(Col, Row, wall);
    cell(Col, Row, visited).

% Check left, right, up, down cells
checkNearCells(Col, Row):-
    (cell(Col, Row, safe) -> safeOrVisited(Col, Row), surroundingSafe(Col, Row); true),
    (cell(Col, Row, stench) -> findElement(Col, Row, wumpus); true),
    (cell(Col, Row, breeze) -> findElement(Col, Row, pit); true),
    (RightCol is Col+1 -> (cell(RightCol, Row, stench), findElement(RightCol, Row, wumpus); true), (cell(RightCol, Row, breeze), findElement(RightCol, Row, pit); true)),
    (LeftCol is Col-1 -> (cell(LeftCol, Row, stench), findElement(LeftCol, Row, wumpus); true), (cell(LeftCol, Row, breeze), findElement(LeftCol, Row, pit); true)),
    (UpRow is Row+1 -> (cell(Col, UpRow, stench), findElement(Col, UpRow, wumpus); true), (cell(Col, UpRow, breeze), findElement(Col, UpRow, pit); true)),
    (DownRow is Col+1 -> (cell(Col, DownRow, stench), findElement(Col, DownRow, wumpus); true), (cell(Col, DownRow, breeze), findElement(Col, DownRow, pit); true)).

% Deduction on what is in right cell
findElement(Col, Row, Z):-
    Row2 is Row-1, safeOrVisited(Col, Row2),
    Row3 is Row+1, safeOrVisited(Col, Row3),
    Col2 is Col-1, safeOrVisited(Col2, Row),
    Col3 is Col+1, \+(cell(Col3, Row, wumpusDead)), % Right
    assertx(cell(Col3, Row, Z)).

% Deduction on what is in left cell
findElement(Col, Row, Z):-
    Row2 is Row-1, safeOrVisited(Col, Row2),
    Row3 is Row+1, safeOrVisited(Col, Row3),
    Col2 is Col+1, safeOrVisited(Col2, Row),
    Col3 is Col-1, \+(cell(Col3, Row, wumpusDead)), % Left
    assertx(cell(Col3, Row, Z)).

% Deduction on what is in up cell
findElement(Col, Row, Z):-
    Col2 is Col+1, safeOrVisited(Col2, Row),
    Col3 is Col-1, safeOrVisited(Col3, Row),
    Row2 is Row-1, safeOrVisited(Col, Row2),
    Row3 is Row+1, \+(cell(Col, Row3, wumpusDead)), % Up
    assertx(cell(Col, Row3, Z)).

% Deduction on what is in down cell
findElement(Col, Row, Z):-
    Col2 is Col+1, safeOrVisited(Col2, Row),
    Col3 is Col-1, safeOrVisited(Col3, Row),
    Row2 is Row+1, safeOrVisited(Col, Row2),
    Row3 is Row-1, \+(cell(Col, Row3, wumpusDead)), % Down
    assertx(cell(Col, Row3, Z)).