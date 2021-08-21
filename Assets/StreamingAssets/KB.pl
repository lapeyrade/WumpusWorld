:- use_module(library(random)).
% :- use_module(library(clpfd)).
% :- use_module(library(reif)).

% :- set_prolog_flag(double_quotes, chars).
:- table cell2/3 as incremental.

:- dynamic([cell/3, nb_wumpus/1, nb_wumpus_dead/1,
            nb_arrow/1, nb_arrow_used/1,
            nb_gold/1, nb_gold_agent/1, wumpus_checked/1],
            [incremental(true)]).

:- discontiguous cell2/3.

% Intermediate Predicate
cell2(Col, Row, Element):- cell(Col, Row, Element).

%%%%%%%%%% NEXT MOVE %%%%%%%%%%

next_move(Move):-
    nb_gold(TotalGold), nb_gold_agent(AgentGold),
    TotalGold == AgentGold,
    Move = "MoveBack", !.

next_move(Move):-
    cell2(Col, Row, agent),
    RightCol is Col+1,
    cell2(RightCol, Row, safe),
    \+(cell2(RightCol, Row, visited)),
    \+(cell2(RightCol, Row, wall)),
    Move = "MoveRight", !.

next_move(Move):-
    cell2(Col, Row, agent),
    LeftCol is Col-1,
    cell2(LeftCol, Row, safe),
    \+(cell2(LeftCol, Row, visited)),
    \+(cell2(LeftCol, Row, wall)),
    Move = "MoveLeft", !.

next_move(Move):-
    cell2(Col, Row, agent),
    UpRow is Row+1,
    cell2(Col, UpRow, safe),
    \+(cell2(Col, UpRow, visited)),
    \+(cell2(Col, UpRow, wall)),
    Move = "MoveUp", !.

next_move(Move):-
    cell2(Col, Row, agent),
    DownRow is Row-1,
    cell2(Col, DownRow, safe),
    \+(cell2(Col, DownRow, visited)),
    \+(cell2(Col, DownRow, wall)),
    Move = "MoveDown", !.

next_move(Move):-
    Move = "MoveBack", !.

% %%%% RANDOM MOVEMENT %%%%
% % Move randomly to a side cell

random_move(Move):-
    random_between(1, 4, Random),
    move(Random, Move).

move(1, Move):-
    Move = "MoveRight".

move(2, Move):-
    Move = "MoveLeft".

move(3, Move):-
    Move = "MoveUp".

move(4, Move):-
    Move = "MoveDown".

%%%%%%%%%% NEXT ACTION %%%%%%%%%%

next_action(Action):-
    cell2(Col, Row, agent),
    cell2(Col, Row, wall),
    Action = "HitWall", !.

next_action(Action):-
    cell2(Col, Row, agent),
    cell2(Col, Row, gold),
    Action = "TakeGold", !.

next_action(Action):-
    Action = "MoveNextCell", !.

%%%%%%%%%% GAME RULES %%%%%%%%%%
% Define Stench & Wumpus attributes
in_limits(Col, Row) :-
	ground([Col, Row]),
    cell2(ColAgent, RowAgent, agent),
    MaxCol is ColAgent + 1,
    MinCol is ColAgent - 1,
    MaxRow is RowAgent + 1,
    MinRow is RowAgent - 1,
    Col > MinCol,
    Col < MaxCol,
    Row > MinRow,
    Row < MaxRow.

% Right
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    RightCol is Col+1,
   	cell2(RightCol, Row, wumpusyes).

% Left
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    LeftCol is Col-1,
   	cell2(LeftCol, Row, wumpusyes).

% Up
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    UpRow is Row+1,
   	cell2(Col, UpRow, wumpusyes).

% Down
cell2(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    DownRow is Row-1,
   	cell2(Col, DownRow, wumpusyes).

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
    in_limits(Col, Row),
    cell2(Col, Row, stenchyes),
    tnot(cell2(Col, Row, stenchno)).

cell2(Col, Row, stench):-
    in_limits(Col, Row),
    tnot(cell2(Col, Row, stenchyes)),
    tnot(cell2(Col, Row, stenchno)),
    tnot(cell2(Col, Row, stench)).

cell2(Col, Row, stench):-
    in_limits(Col, Row),
    cell2(Col, Row, stenchyes),
    cell2(Col, Row, stenchno),
    tnot(cell2(Col, Row, stench)).

% Right
cell2(Col, Row, wumpusno) :-
    % in_limits(Col, Row),
    NewCol is Col+1,
   	cell2(NewCol, Row, stenchno).

% Left
cell2(Col, Row, wumpusno) :-
    % in_limits(Col, Row),
    NewCol is Col-1,
   	cell2(NewCol, Row, stenchno).

% Up
cell2(Col, Row, wumpusno) :-
    % in_limits(Col, Row),
    NewRow is Row+1,
   	cell2(Col, NewRow, stenchno).

% Down
cell2(Col, Row, wumpusno) :-
    % in_limits(Col, Row),
    NewRow is Row-1,
    cell2(Col, NewRow, stenchno).

% Right
cell2(Col, Row, wumpusyes) :-
    % in_limits(Col, Row),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
    cell2(RightCol, Row, stenchyes),
	cell2(FarRightCol, Row, wumpusno),
	cell2(RightCol, UpRow, wumpusno),
	cell2(RightCol, DownRow, wumpusno).

% Left
cell2(Col, Row, wumpusyes) :-
    % in_limits(Col, Row),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
    cell2(LeftCol, Row, stenchyes),
	cell2(FarLeftCol, Row, wumpusno),
	cell2(LeftCol, UpRow, wumpusno),
	cell2(LeftCol, DownRow, wumpusno).

% Up
cell2(Col, Row, wumpusyes) :-
    % in_limits(Col, Row),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
    cell2(Col, UpRow, stenchyes),
	cell2(Col, FarUpRow, wumpusno),
	cell2(LeftCol, UpRow, wumpusno),
	cell2(RightCol, UpRow, wumpusno).

% Down
cell2(Col, Row, wumpusyes) :-
    % in_limits(Col, Row),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
    cell2(Col, DownRow, stenchyes),
	cell2(Col, FarDownRow, wumpusno),
	cell2(LeftCol, DownRow, wumpusno),
	cell2(RightCol, DownRow, wumpusno).

cell2(Col, Row, wumpus):-
    % in_limits(Col, Row),
    cell2(Col, Row, wumpusyes),
    tnot(cell2(Col, Row, wumpusno)).

cell2(Col, Row, wumpus):-
    % in_limits(Col, Row),
    tnot(cell2(Col, Row, wumpusyes)),
    tnot(cell2(Col, Row, wumpusno)),
    tnot(cell2(Col, Row, wumpus)).

cell2(Col, Row, wumpus):-
    % in_limits(Col, Row),
    cell2(Col, Row, wumpusyes),
    cell2(Col, Row, wumpusno),
    tnot(cell2(Col, Row, wumpus)).

cell2(Col, Row, safe):-
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    (
        cell2(Col, Row, wumpusno);
        cell2(RightCol, Row, stenchno);
        cell2(LeftCol, Row, stenchno);
        cell2(Col, UpRow, stenchno);
        cell2(Col, DownRow, stenchno)
    ),
    (
        cell2(Col, Row, pitno);
        cell2(RightCol, Row, breezeno);
        cell2(LeftCol, Row, breezeno);
        cell2(Col, UpRow, breezeno);
        cell2(Col, DownRow, breezeno) 
    ).
