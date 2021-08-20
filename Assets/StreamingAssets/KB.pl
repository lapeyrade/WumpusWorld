:- use_module(library(random)).
% :- use_module(library(clpfd)).
% :- use_module(library(reif)).

% :- set_prolog_flag(double_quotes, chars).
:- table cell2/3 as incremental.

:- dynamic([cell/3, nb_wumpus/1, nb_wumpus_dead/1,
            nb_arrow/1, nb_arrow_used/1,
            nb_gold/1, nb_gold_agent/1, wumpus_checked/1],
            [incremental(true)]).

:- discontiguous cell/3.

%%%%%%%%%% NEXT MOVE %%%%%%%%%%

next_move(Move):-
    nb_gold(TotalGold), nb_gold_agent(AgentGold),
    TotalGold == AgentGold,
    Move = "MoveBack", !.

next_move(Move):-
    cell(Col, Row, agent),
    RightCol is Col+1,
    cell(RightCol, Row, safe),
    \+(cell(RightCol, Row, visited)),
    \+(cell(RightCol, Row, wall)),
    Move = "MoveRight", !.

next_move(Move):-
    cell(Col, Row, agent),
    LeftCol is Col-1,
    cell(LeftCol, Row, safe),
    \+(cell(LeftCol, Row, visited)),
    \+(cell(LeftCol, Row, wall)),
    Move = "MoveLeft", !.

next_move(Move):-
    cell(Col, Row, agent),
    UpRow is Row+1,
    cell(Col, UpRow, safe),
    \+(cell(Col, UpRow, visited)),
    \+(cell(Col, UpRow, wall)),
    Move = "MoveUp", !.

next_move(Move):-
    cell(Col, Row, agent),
    DownRow is Row-1,
    cell(Col, DownRow, safe),
    \+(cell(Col, DownRow, visited)),
    \+(cell(Col, DownRow, wall)),
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
    cell(Col, Row, agent),
    cell(Col, Row, wall),
    Action = "HitWall", !.

next_action(Action):-
    cell(Col, Row, agent),
    cell(Col, Row, gold),
    Action = "TakeGold", !.

% cell(2, 3, wumpusyes):-
%     cell(1, 1, agent).

% cell(3, 3, wumpusyes):-
%     cell(1, 1, agent).

% cell(X, Y, safe):-
%     tnot(cell(Col, Row, wumpus)),
%     tnot(cell(Col, Row, pit)).

% cell(1, 1, safe).
% cell(1, 1, stenchno).
% cell(1, 1, wumpusno).

next_action(Action):-
    wumpus_checked(false),
    Action = "CheckForWumpus",
    retractall(wumpus_checked(_)),
    assertz(wumpus_checked(true)), !.

next_action(Action):-
    retractall(wumpus_checked(_)),
    assertz(wumpus_checked(false)),
    Action = "MoveNextCell", !.

%%%%%%%%%% GAME RULES %%%%%%%%%%
% Define Stench & Wumpus attributes

in_limits(Col, Row) :-
	ground([Col, Row]),
    cell(ColAgent, RowAgent, agent),
    MaxCol is ColAgent + 3,
    MinCol is ColAgent - 3,
    MaxRow is RowAgent + 3,
    MinRow is RowAgent - 3,
    Col > MinCol,
    Col < MaxCol,
    Row > MinRow,
    Row < MaxRow.

% Right
cell(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    NewCol is Col+1,
   	cell(NewCol, Row, wumpusyes).

% Left
cell(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    NewCol is Col-1,
   	cell(NewCol, Row, wumpusyes).

% Up
cell(Col, Row, stenchyes) :-
    in_limits(Col, Row),
    NewRow is Row+1,
   	cell(Col, NewRow, wumpusyes).

% Down
cell(Col, Row, stenchyes) :-
    in_limits(Col, Row), 
    NewRow is Row-1,
   	cell(Col, NewRow, wumpusyes).

cell(Col, Row, stenchno) :-
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	cell(RightCol, Row, wumpusno),
	cell(LeftCol, Row, wumpusno),
	cell(Col, UpRow, wumpusno),
	cell(Col, DownRow, wumpusno).

% Right
cell(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewCol is Col+1,
   	cell(NewCol, Row, stenchno),
    cell(NewCol, Row, wumpusno).

% Left
cell(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewCol is Col-1,
   	cell(NewCol, Row, stenchno).

% Up
cell(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewRow is Row+1,
   	cell(Col, NewRow, stenchno).

% Down
cell(Col, Row, wumpusno) :-
    in_limits(Col, Row),
    NewRow is Row-1,
    cell(Col, NewRow, stenchno).

% Right
cell(Col, Row, wumpusyes) :-
    % write(Col), writeln(Row),
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    cell(RightCol, Row, stenchyes),
	% tnot(cell(RightCol, Row, wumpusyes)),
	tnot(cell(LeftCol, Row, wumpusyes)),
	tnot(cell(Col, UpRow, wumpusyes)),
	tnot(cell(Col, DownRow, wumpusyes)).

% Left
cell(Col, Row, wumpusyes) :-
    % write(Col), writeln(Row),
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    cell(LeftCol, Row, stenchyes),
	tnot(cell(RightCol, Row, wumpusyes)),
	% tnot(cell(LeftCol, Row, wumpusyes)),
	tnot(cell(Col, UpRow, wumpusyes)),
	tnot(cell(Col, DownRow, wumpusyes)).

% Up
cell(Col, Row, wumpusyes) :-
    % write(Col), writeln(Row), 
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    cell(Col, UpRow, stenchyes),
	tnot(cell(RightCol, Row, wumpusyes)),
	tnot(cell(LeftCol, Row, wumpusyes)),
	% tnot(cell(Col, UpRow, wumpusyes)),
	tnot(cell(Col, DownRow, wumpusyes)).

% Down
cell(Col, Row, wumpusyes) :-
    % write(Col), writeln(Row),
    in_limits(Col, Row),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    cell(Col, DownRow, stenchyes),
	tnot(cell(RightCol, Row, wumpusyes)),
	tnot(cell(LeftCol, Row, wumpusyes)),
	tnot(cell(Col, UpRow, wumpusyes)).
	% tnot(cell(Col, DownRow, wumpusyes)).

cell(Col, Row, wumpus):-
    in_limits(Col, Row),
    cell(Col, Row, wumpusyes),
    tnot(cell(Col, Row, wumpusno)).

cell(Col, Row, wumpus):-
    in_limits(Col, Row),
    tnot(cell(Col, Row, wumpusyes)),
    tnot(cell(Col, Row, wumpusno)),
    tnot(cell(Col, Row, wumpus)).

cell(Col, Row, wumpus):-
    in_limits(Col, Row),
    cell(Col, Row, wumpusyes),
    cell(Col, Row, wumpusno),
    tnot(cell(Col, Row, wumpus)).

cell(Col, Row, stench):-
    in_limits(Col, Row),
    cell(Col, Row, stenchyes),
    tnot(cell(Col, Row, stenchno)).

cell(Col, Row, stench):-
    in_limits(Col, Row),
    tnot(cell(Col, Row, stenchyes)),
    tnot(cell(Col, Row, stenchno)),
    tnot(cell(Col, Row, stench)).

cell(Col, Row, stench):-
    in_limits(Col, Row),
    cell(Col, Row, stenchyes),
    cell(Col, Row, stenchno),
    tnot(cell(Col, Row, stench)).

cell2(Col, Row, safe):-
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    (cell(Col, Row, wumpusno);
        cell(RightCol, Row, stenchno);
        cell(LeftCol, Row, stenchno);
        cell(Col, UpRow, stenchno);
        cell(Col, DownRow, stenchno)).