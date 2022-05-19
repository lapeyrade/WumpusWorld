:- module(cell, [cell2/3, cell/3]).

:- table cell2/3 as incremental.

:- dynamic([cell/3], [incremental(true)]).

%%%%%%%%%% CELL ONTOLOGY %%%%%%%%%%

% Intermediate Predicate
cell2(Col, Row, Element):- cell(Col, Row, Element).

cell2(Col, Row, wumpusno):-
    in_limits(Col, Row),
    cell2(Col, Row, visited),
    (
        is_false(cell2(Col, Row, wumpusyes)),
        is_false(cell2(Col, Row, wumpusdead))
    ).

cell2(Col, Row, pitno):-
    in_limits(Col, Row),
    cell2(Col, Row, visited),
    is_false(cell2(Col, Row, pityes)).

cell2(Col, Row, breezeno):-
    in_limits(Col, Row),
    cell2(Col, Row, visited),
    is_false(cell2(Col, Row, breezeyes)).

cell2(Col, Row, stenchno):-
    in_limits(Col, Row),
    cell2(Col, Row, visited),
    is_false(cell2(Col, Row, stenchyes)).

cell2(Col, Row, safe):-
    cell2(Col, Row, wall).

cell2(Col, Row, safe):-
    cell2(Col, Row, wumpusdead).

cell2(Col, Row, safe):-
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    (
        is_false(cell2(Col, Row, wumpus));
        is_false(cell2(RightCol, Row, stench));
        is_false(cell2(LeftCol, Row, stench));
        is_false(cell2(Col, UpRow, stench));
        is_false(cell2(Col, DownRow, stench))
    ),
    (
        is_false(cell2(Col, Row, pit));
        is_false(cell2(RightCol, Row, breeze));
        is_false(cell2(LeftCol, Row, breeze));
        is_false(cell2(Col, UpRow, breeze));
        is_false(cell2(Col, DownRow, breeze))
    ).

%%% Define Stench & Wumpus attributes %%%
% Right
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    RightCol is Col+1,
   	(
        cell2(RightCol, Row, wumpusyes);
        cell2(RightCol, Row, wumpusdead)
    ).

% Left
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    LeftCol is Col-1,
    (
        cell2(LeftCol, Row, wumpusyes);
        cell2(LeftCol, Row, wumpusdead)
    ).

% Up
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    UpRow is Row+1,
    (
        cell2(Col, UpRow, wumpusyes);
        cell2(Col, UpRow, wumpusdead)
    ).

% Down
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    DownRow is Row-1,
    (
        cell2(Col, DownRow, wumpusyes);
        cell2(Col, DownRow, wumpusdead)
    ).

cell2(Col, Row, stenchno) :-
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(RightCol, Row, wumpusno),
	cell2(LeftCol, Row, wumpusno),
	cell2(Col, UpRow, wumpusno),
	cell2(Col, DownRow, wumpusno).

cell2(Col, Row, stench):-
    cell2(Col, Row, stenchyes),
    tnot(cell2(Col, Row, stenchno)).

cell2(Col, Row, stench):-
    tnot(cell2(Col, Row, stenchyes)),
    tnot(cell2(Col, Row, stenchno)),
    tnot(cell2(Col, Row, stench)).

cell2(Col, Row, stench):-
    cell2(Col, Row, stenchyes),
    cell2(Col, Row, stenchno),
    tnot(cell2(Col, Row, stench)).

% Right
cell2(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewCol is Col+1,
   	cell2(NewCol, Row, stenchno).

% Left
cell2(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewCol is Col-1,
   	cell2(NewCol, Row, stenchno).

% Up
cell2(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewRow is Row+1,
   	cell2(Col, NewRow, stenchno).

% Down
cell2(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewRow is Row-1,
    cell2(Col, NewRow, stenchno).

% Right
cell2(Col, Row, wumpusyes) :-
    \+(cell2(Col, Row, wumpusdead)),
    in_limits(Col, Row),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(FarRightCol, Row, wumpusno),
	cell2(RightCol, UpRow, wumpusno),
	cell2(RightCol, DownRow, wumpusno), 
    cell2(RightCol, Row, stenchyes).

% Left
cell2(Col, Row, wumpusyes) :-
    \+(cell2(Col, Row, wumpusdead)),
    in_limits(Col, Row),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(FarLeftCol, Row, wumpusno),
	cell2(LeftCol, UpRow, wumpusno), 
	cell2(LeftCol, DownRow, wumpusno), 
    cell2(LeftCol, Row, stenchyes).

% Up
cell2(Col, Row, wumpusyes) :-
    \+(cell2(Col, Row, wumpusdead)),
    in_limits(Col, Row),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2(Col, FarUpRow, wumpusno),
	cell2(LeftCol, UpRow, wumpusno),
	cell2(RightCol, UpRow, wumpusno),
    cell2(Col, UpRow, stenchyes).

% Down
cell2(Col, Row, wumpusyes) :-
    \+(cell2(Col, Row, wumpusdead)),
    in_limits(Col, Row),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2(Col, FarDownRow, wumpusno),
	cell2(LeftCol, DownRow, wumpusno),
	cell2(RightCol, DownRow, wumpusno),
    cell2(Col, DownRow, stenchyes).

cell2(Col, Row, wumpus):-
    in_limits(Col, Row),
    cell2(Col, Row, wumpusyes),
    tnot(cell2(Col, Row, wumpusno)).

cell2(Col, Row, wumpus):-
    in_limits(Col, Row),
    tnot(cell2(Col, Row, wumpusyes)),
    tnot(cell2(Col, Row, wumpusno)),
    tnot(cell2(Col, Row, wumpus)).

cell2(Col, Row, wumpus):-
    in_limits(Col, Row),
    cell2(Col, Row, wumpusyes),
    cell2(Col, Row, wumpusno),
    tnot(cell2(Col, Row, wumpus)).

%%% Define Breeze & Pit attributes %%%
cell2(Col, Row, breezeyes) :-
    in_limits(Col, Row),
    RightCol is Col+1,
   	cell2(RightCol, Row, pityes).

% Left
cell2(Col, Row, breezeyes) :-
    in_limits(Col, Row),
    LeftCol is Col-1,
   	cell2(LeftCol, Row, pityes).

% Up
cell2(Col, Row, breezeyes) :-
    in_limits(Col, Row),
    UpRow is Row+1,
   	cell2(Col, UpRow, pityes).

% Down
cell2(Col, Row, breezeyes) :-
    in_limits(Col, Row),
    DownRow is Row-1,
   	cell2(Col, DownRow, pityes).

cell2(Col, Row, breezeno) :-
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(RightCol, Row, pitno),
	cell2(LeftCol, Row, pitno),
	cell2(Col, UpRow, pitno),
	cell2(Col, DownRow, pitno).

cell2(Col, Row, breeze):-
    cell2(Col, Row, breezeyes),
    tnot(cell2(Col, Row, breezeno)).

cell2(Col, Row, breeze):-
    tnot(cell2(Col, Row, breezeyes)),
    tnot(cell2(Col, Row, breezeno)),
    tnot(cell2(Col, Row, breeze)).

cell2(Col, Row, breeze):-
    cell2(Col, Row, breezeyes),
    cell2(Col, Row, breezeno),
    tnot(cell2(Col, Row, breeze)).

% Right
cell2(Col, Row, pitno) :-
    in_limits(Col, Row),
    NewCol is Col+1,
   	cell2(NewCol, Row, breezeno).

% Left
cell2(Col, Row, pitno) :-
    in_limits(Col, Row),
    NewCol is Col-1,
   	cell2(NewCol, Row, breezeno).

% Up
cell2(Col, Row, pitno) :-
    in_limits(Col, Row),
    NewRow is Row+1,
   	cell2(Col, NewRow, breezeno).

% Down
cell2(Col, Row, pitno) :-
    in_limits(Col, Row),
    NewRow is Row-1,
    cell2(Col, NewRow, breezeno).

% Right
cell2(Col, Row, pityes) :-
    in_limits(Col, Row),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(FarRightCol, Row, pitno),
	cell2(RightCol, UpRow, pitno),
	cell2(RightCol, DownRow, pitno), 
    cell2(RightCol, Row, breezeyes).

% Left
cell2(Col, Row, pityes) :-
    in_limits(Col, Row),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2(FarLeftCol, Row, pitno),
	cell2(LeftCol, UpRow, pitno), 
	cell2(LeftCol, DownRow, pitno), 
    cell2(LeftCol, Row, breezeyes).

% Up
cell2(Col, Row, pityes) :-
    in_limits(Col, Row),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2(Col, FarUpRow, pitno),
	cell2(LeftCol, UpRow, pitno),
	cell2(RightCol, UpRow, pitno),
    cell2(Col, UpRow, breezeyes).

% Down
cell2(Col, Row, pityes) :-
    in_limits(Col, Row),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2(Col, FarDownRow, pitno),
	cell2(LeftCol, DownRow, pitno),
	cell2(RightCol, DownRow, pitno),
    cell2(Col, DownRow, breezeyes).

cell2(Col, Row, pit):-
    in_limits(Col, Row),
    cell2(Col, Row, pityes),
    tnot(cell2(Col, Row, pitno)).

cell2(Col, Row, pit):-
    in_limits(Col, Row),
    tnot(cell2(Col, Row, pityes)),
    tnot(cell2(Col, Row, pitno)),
    tnot(cell2(Col, Row, pit)).

cell2(Col, Row, pit):-
    in_limits(Col, Row),
    cell2(Col, Row, pityes),
    cell2(Col, Row, pitno),
    tnot(cell2(Col, Row, pit)).

in_limits(Col, Row) :-
    cell2(ColAgent, RowAgent, agent),
    MinCol is ColAgent - 3, MaxCol is ColAgent + 3,
    MinRow is RowAgent - 3, MaxRow is RowAgent + 3,
    numlist(MinCol, MaxCol, ListCol),
    numlist(MinRow, MaxRow, ListRow),
    member(Col, ListCol),
    member(Row, ListRow).