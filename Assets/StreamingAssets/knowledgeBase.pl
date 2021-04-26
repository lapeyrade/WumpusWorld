:- use_module(library(random)).

:- dynamic([cell/3, world/3, nbWumpus/1, nbWumpusDead/1,
            nbArrow/1, nbArrowUsed/1, nbGold/1, nbGoldAgent/1,
            allGoldsFound/1, gameOver/1, stackCol/1, stackRow/1]).

%%%%%%%%%% CUSTOM PREDICATES %%%%%%%%%%
% Check if fact exists, if not assert it
assertX(Fact):-
    \+(Fact) -> assertz(Fact); true.

retractX(Fact):-
    Fact -> retract(Fact); true.

updateX(OldPredicate, NewPredicate):-
    retractX(OldPredicate),
    assertX(NewPredicate).

%%%%%%%%%% KNOWLEDGEBASE MANAGEMENT %%%%%%%%%%
% Erase all knowledges
resetKB():-
    retractall(cell(_, _, _)),
    retractall(world(_, _, _)),
    retractall(nbWumpus(_, _, _)),
    retractall(nbWumpusDead(_, _, _)),
    retractall(nbArrow(_, _, _)),
    retractall(nbArrowUsed(_, _, _)),
    retractall(nbGold(_, _, _)),
    retractall(nbGoldAgent(_, _, _)),
    retractall(allGoldsFound(_)),
    retractall(gameOver(_)),
    retractall(stackCol(_)),
    retractall(stackRow(_)).

% Initiate the game with attributes  Ex: initGame(0, 1, 1, 0, 0, 8, 8, 1, 1, 3).
initGame(RandSeed, StartCol, StartRow, MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NBGold, NBWumpus, NBPit):-
    set_random(seed(RandSeed)),
    initAgent(StartCol, StartRow),
    initGameAttributes(NBWumpus, NBGold),
    assertX(world(StartCol, StartRow, start)),
    initWallCol(MinGridCol, MinGridRow, MaxGridRow); % Left
    initWallCol(MaxGridCol, MinGridRow, MaxGridRow); % Right
    initWallRow(MinGridCol, MinGridRow, MaxGridCol); % Bot
    initWallRow(MinGridCol, MaxGridRow, MaxGridCol); % Top
    initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBGold);
    initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBWumpus);
    initPit(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, 0, NBPit).

% Initiate attributes values
initGameAttributes(NBWumpus, NBGold):-
    assertX(nbWumpus(NBWumpus)),
    assertX(nbWumpusDead(0)),
    assertX(nbArrow(NBWumpus)),
    assertX(nbArrowUsed(0)),
    assertX(nbGold(NBGold)),
    assertX(nbGoldAgent(0)),
    assertX(allGoldsFound(false)),
    assertX(gameOver(false)).

% Initiate agent attributes
initAgent(Col, Row):-
    assertX(cell(Col, Row, start)),
    assertX(cell(Col, Row, agent)),
    assertX(cell(Col, Row, visited)),
    assertX(stackCol([Col])),
    assertX(stackRow([Row])),
    surroundingSafe(Col, Row).

% Initiate Top & Bottom Walls
initWallRow(Col, Row, MaxGridCol):-
    assertX(world(Col, Row, wall)); Col < MaxGridCol,
    NewCol is Col + 1, initWallRow(NewCol, Row, MaxGridCol).

% Initiate Left & Right Walls
initWallCol(Col, Row, MaxGridRow):-
    assertX(world(Col, Row, wall)); Row < MaxGridRow,
    NewRow is Row + 1, initWallCol(Col, NewRow, MaxGridRow).

% Initiate Golds Ex: initGold(0, 0, 4, 4, 0, 1).
initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Gold, NBGold):-
    Gold < NBGold,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertX(world(NewCol, NewRow, gold)),
    NewGold is Gold + 1); NewGold is Gold),
    initGold(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NewGold, NBGold).

% Initiate Wumpus Ex: initWumpus(0, 0, 4, 4, 0, 1).
initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Wumpus, NBWumpus):-
    Wumpus < NBWumpus,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertX(world(NewCol, NewRow, wumpus)),
    NewWumpus is Wumpus + 1, generateAroundCell(NewCol, NewRow, stench)); NewWumpus is Wumpus),
    initWumpus(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, NewWumpus, NBWumpus).

% Initiate Pits Ex: initPit(0, 0, 4, 4, 0, 3).
initPit(MinGridCol, MinGridRow, MaxGridCol, MaxGridRow, Pit, NBPit):-
    Pit < NBPit,
    random_between(MinGridCol, MaxGridCol, NewCol),
    random_between(MinGridRow, MaxGridRow, NewRow),
    (cellFree(NewCol, NewRow) -> (assertX(world(NewCol, NewRow, pit)),
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
    RightCol is Col+1, assertX(world(RightCol, Row, State)),
    LeftCol is Col-1, assertX(world(LeftCol, Row, State)),
    UpRow is Row+1, assertX(world(Col, UpRow, State)),
    DownRow is Row-1, assertX(world(Col, DownRow, State)).

%%%%%%%%%% STACK MANAGEMENT %%%%%%%%%%
% Add Element at the 1st place of stack
push(Element, Stack, [Element|Stack]).
% Remove 1st element from stack
pop(Element, [Element|Stack], Stack).

% Add Coordinate to stack
pushStack(NewCol, NewRow):-
    stackCol(StackCol), stackRow(StackRow),
    push(NewCol, StackCol, NewStackCol),
    push(NewRow, StackRow, NewStackRow),
    updateX(stackCol(StackCol), stackCol(NewStackCol)),
    updateX(stackRow(StackRow), stackRow(NewStackRow)).

% Show 1st Element from stack
peekStack(TopCol, TopRow):-
    stackCol(StackCol), stackRow(StackRow),
    push(TopCol, _, StackCol), push(TopRow, _, StackRow).

% Remove 1st Element from stack
popStack():-
    stackCol(StackCol), stackRow(StackRow),
    pop(_, StackCol, NewStackCol),
    pop(_, StackRow, NewStackRow),
    updateX(stackCol(StackCol), stackCol(NewStackCol)),
    updateX(stackRow(StackRow), stackRow(NewStackRow)).

%%%%%%%%%% AGENT MOVEMENT %%%%%%%%%%

%%%% DETERMINISTIC MOVEMENT %%%%

% If cell safe -> mark as safe & choose next move
nextMoveProlog(Col, Row, NewCol, NewRow):-
    checkCell(Col, Row),
    prologMove(Col, Row, NewCol, NewRow),
    checkNextCell(Col, Row, NewCol, NewRow).

nextMovePlayer(Col, Row, NewCol, NewRow):-
    checkCell(Col, Row),
    pushStack(NewCol, NewRow),
    checkNextCell(stackCol, Row, NewCol, NewRow).

% Go back to start cell if all golds are found
prologMove(_, _, NewCol, NewRow):-
    allGoldsFound(true),
    popStack,
    peekStack(PrevCol, PrevRow),
    NewCol is PrevCol, NewRow is PrevRow.

% If right cell contains dead wumpus & not visited go right
prologMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewCol is Col+1, NewRow is Row, % Right
    cell(NewCol, NewRow, wumpusDead),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If left cell contains dead wumpus & not visited go left
prologMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewCol is Col-1, NewRow is Row, % left
    cell(NewCol, NewRow, wumpusDead),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If up cell contains dead wumpus & not visited go up
prologMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewRow is Row+1, NewCol is Col, % Up
    cell(NewCol, NewRow, wumpusDead),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If down cell contains dead wumpus & not visited go down
prologMove(Col, Row, NewCol, NewRow):-
    cell(Col, Row, stench),
    NewRow is Row-1, NewCol is Col, % Down
    cell(NewCol, NewRow, wumpusDead),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If right cell safe & not visited & not wall -> go right
prologMove(Col, Row, NewCol, NewRow):-
    NewCol is Col+1, NewRow is Row, % Right
    safeOrStart(NewCol, NewRow),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If left cell safe & not visited & not wall -> go left
prologMove(Col, Row, NewCol, NewRow):-
    NewCol is Col-1, NewRow is Row, % Left
    safeOrStart(NewCol, NewRow),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If up cell safe & not visited & not wall -> go up
prologMove(Col, Row, NewCol, NewRow):-
    NewRow is Row+1, NewCol is Col, % Up
    safeOrStart(NewCol, NewRow),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% If down cell safe & not visited & not wall -> go down
prologMove(Col, Row, NewCol, NewRow):-
    NewRow is Row-1, NewCol is Col, % Down
    safeOrStart(NewCol, NewRow),
    \+(wallOrVisited(NewCol, NewRow)),
    pushStack(NewCol, NewRow).

% Otherwise go to previous cell
prologMove(_, _, NewCol, NewRow):-
    popStack, peekStack(PrevCol, PrevRow),
    NewCol is PrevCol, NewRow is PrevRow.

checkCell(Col, Row):-
    (cell(Col, Row, start), allGoldsFound(true)) ->
        (updateX(gameOver(_) , gameOver(true)), false); true,
    (cell(Col, Row, pit) ; cell(Col, Row, wumpus)) ->
        (updateX(gameOver(_) , gameOver(true)), false); true,
    (cell(Col, Row, gold) -> 
        nbGoldAgent(NBGoldAgent), retractX(world(Col, Row, gold)),
        retractX(cell(Col, Row, gold)), NewNBGoldAgent is NBGoldAgent + 1,
        updateX(nbGoldAgent(_), nbGoldAgent(NewNBGoldAgent)),
        nbGold(NBGold), (NewNBGoldAgent = NBGold ->
            updateX(allGoldsFound(_), allGoldsFound(true)); true); true),
    seekWumpus(Col, Row).

seekWumpus(Col, Row):-
    RightCol is Col+1, LeftCol is Col-1, DownRow is Row-1, UpRow is Row+1,
    RightCol2 is Col+2, LeftCol2 is Col-2, DownRow2 is Row-2, UpRow2 is Row+2,
    killWumpus(RightCol, Row),
    killWumpus(RightCol2, Row),
    killWumpus(LeftCol, Row),
    killWumpus(LeftCol2, Row),
    killWumpus(Col, UpRow),
    killWumpus(Col, UpRow2),
    killWumpus(Col, DownRow),
    killWumpus(Col, DownRow2).

killWumpus(Col, Row):-
    (cell(Col, Row, wumpus), nbArrow(ArrowAgent), ArrowAgent > 0) ->
        (retractX(cell(Col, Row, wumpus)), retractX(world(Col, Row, wumpus)),
        assertX(cell(Col, Row, wumpusDead)), assertX(world(Col, Row, wumpusDead)), nbWumpusDead(NBWumpusDead), retractX(nbWumpusDead(NBWumpusDead)), 
        assertX(cell(Col, Row, safe)), NewArrowAgent is ArrowAgent - 1,
        updateX(nbArrow(_), nbArrow(NewArrowAgent))); true.
        
% Sense what is in destination cell
checkNextCell(Col, Row, NewCol, NewRow):-
    (world(NewCol, NewRow, wall) ->
        (assertX(cell(NewCol, NewRow, wall)), retractX(cell(NewCol, NewRow, safe)), popStack);
            (updateX(cell(Col, Row, agent), cell(NewCol, NewRow, agent)), assertX(cell(NewCol, NewRow, visited)))),
    (world(NewCol, NewRow, breeze) ->
        (retractX(cell(NewCol, NewRow, safe)), assertX(cell(NewCol, NewRow, breeze))); true),
    (world(NewCol, NewRow, stench) ->
        (retractX(cell(NewCol, NewRow, safe)), assertX(cell(NewCol, NewRow, stench))); true),
    (world(NewCol, NewRow, gold) ->
        (assertX(cell(NewCol, NewRow, gold))); true),
    (world(NewCol, NewRow, pit) ->
        (assertX(cell(NewCol, NewRow, pit)), (updateX(gameOver(_), gameOver(true)))); true),
    (world(NewCol, NewRow, wumpus) ->
        (assertX(cell(NewCol, NewRow, wumpus)), (updateX(gameOver(_), gameOver(true))));true),
    checkNearCells(NewCol, NewRow).
    
%%%% RANDOM MOVEMENT %%%%
% Move randomly to a side cell
randomMove(Col, Row, NewCol, NewRow):-
    checkCell(Row, Col),
    random_between(1, 4, Random),
    move(Col, Row, NewCol, NewRow, Random),
    checkNextCell(Col, Row, NewCol, NewRow).

% Move right
move(Col, Row, NewCol, NewRow, 1):- % Right
    NewCol is Col+1, NewRow is Row,
    pushStack(NewCol, NewRow).

% Move left
move(Col, Row, NewCol, NewRow, 2):- % Left
    NewCol is Col-1, NewRow is Row,
    pushStack(NewCol, NewRow).

% Move down
move(Col, Row, NewCol, NewRow, 3):- % Down
    NewRow is Row-1, NewCol is Col,
    pushStack(NewCol, NewRow).

% Move up
move(Col, Row, NewCol, NewRow, 4):- % Up
    NewRow is Row+1, NewCol is Col,
    pushStack(NewCol, NewRow).

%%%%%%%%%% NEAR CELL DETECTION %%%%%%%%%%

isSafe(Col, Row):-
    (\+(cell(Col, Row, breeze)),
    \+(cell(Col, Row, wall)),
    \+(cell(Col, Row, stench)),
    \+(cell(Col, Row, wumpus)),
    \+(cell(Col, Row, pit))) ->
        (assertX(cell(Col, Row, safe))); false.

% Check if side cells are safe
surroundingSafe(Col, Row):-
    Col2 is Col+1,  markSafe(Col2, Row),
    Col3 is Col-1,  markSafe(Col3, Row),
    Row2 is Row+1,  markSafe(Col, Row2),
    Row3 is Row-1,  markSafe(Col, Row3).

% If cell not visited nor wall, mark it safe 
markSafe(Col, Row):-
    (\+(cell(Col, Row, visited)),
    \+(cell(Col, Row, wall))) ->
        (assertX(cell(Col, Row, safe))); true.

% Check is cell is safe or start
safeOrStart(Col, Row):-
    cell(Col, Row, start); cell(Col, Row, safe).

% Check is cell is safe or visited and without dead wumpus
safeOrVisited(Col, Row):-
    \+(cell(Col, Row, wumpusDead)),
    (cell(Col, Row, visited); cell(Col, Row, safe)).

% Check is cell is wall or visited
wallOrVisited(Col, Row):-
    cell(Col, Row, wall);
    cell(Col, Row, visited).

% Check left, right, up, down cells
checkNearCells(Col, Row):-
    (isSafe(Col, Row) -> surroundingSafe(Col, Row); true),
    (cell(Col, Row, stench) -> findElement(Col, Row, wumpus); true),
    (cell(Col, Row, breeze) -> findElement(Col, Row, pit); true),
    RightCol is Col+1, 
        (cell(RightCol, Row, stench) -> findElement(RightCol, Row, wumpus); true),
        (cell(RightCol, Row, breeze) -> findElement(RightCol, Row, pit); true),
    LeftCol is Col-1,
        (cell(LeftCol, Row, stench) -> findElement(LeftCol, Row, wumpus); true),
        (cell(LeftCol, Row, breeze) -> findElement(LeftCol, Row, pit); true),
    UpRow is Row+1,
        (cell(Col, UpRow, stench) -> findElement(Col, UpRow, wumpus); true),
        (cell(Col, UpRow, breeze) -> findElement(Col, UpRow, pit); true),
    DownRow is Row-1,
        (cell(Col, DownRow, stench) -> findElement(Col, DownRow, wumpus); true),
        (cell(Col, DownRow, breeze) -> findElement(Col, DownRow, pit); true).

findElement(Col, Row, State):-
    RightCol is Col+1, LeftCol is Col-1,
    DownRow is Row-1, UpRow is Row+1,
    ((safeOrVisited(LeftCol, Row), safeOrVisited(Col, DownRow), safeOrVisited(Col, UpRow), \+(cell(RightCol, Row, wumpusDead))) -> assertX(cell(RightCol, Row, State)); true), % Right
    ((safeOrVisited(RightCol, Row), safeOrVisited(Col, DownRow), safeOrVisited(Col, UpRow), \+(cell(LeftCol, Row, wumpusDead))) -> assertX(cell(LeftCol, Row, State)); true), % Left
    ((safeOrVisited(RightCol, Row), safeOrVisited(LeftCol, Row), safeOrVisited(Col, UpRow), \+(cell(Col, DownRow, wumpusDead))) -> assertX(cell(Col, DownRow, State)); true), % Up
    ((safeOrVisited(RightCol, Row), safeOrVisited(LeftCol, Row), safeOrVisited(Col, DownRow), \+(cell(Col, UpRow, wumpusDead))) -> assertX(cell(Col, UpRow, State)); true). % Down