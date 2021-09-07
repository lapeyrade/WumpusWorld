:- use_module(library(random)).
% :- use_module(library(clpfd)).
% :- use_module(library(reif)).

% :- set_prolog_flag(double_quotes, chars).

:- set_prolog_flag(toplevel_list_wfs_residual_program, false).

:- table cell2/3 as incremental.

:- dynamic([cell/3, nb_wumpus/1, nb_wumpus_dead/1,
            nb_arrow/1, nb_arrow_used/1,
            nb_gold/1, nb_gold_agent/1, grid_coord/4],
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
    \+(cell2(RightCol, Row, visited)),
    \+(cell2(RightCol, Row, wall)),
    cell2(RightCol, Row, safe),
    Move = "MoveRight", !.

next_move(Move):-
    cell2(Col, Row, agent),
    LeftCol is Col-1,
    \+(cell2(LeftCol, Row, visited)),
    \+(cell2(LeftCol, Row, wall)),
    cell2(LeftCol, Row, safe),
    Move = "MoveLeft", !.

next_move(Move):-
    cell2(Col, Row, agent),
    UpRow is Row+1,
    \+(cell2(Col, UpRow, visited)),
    \+(cell2(Col, UpRow, wall)),
    cell2(Col, UpRow, safe),
    Move = "MoveUp", !.

next_move(Move):-
    cell2(Col, Row, agent),
    DownRow is Row-1,
    \+(cell2(Col, DownRow, visited)),
    \+(cell2(Col, DownRow, wall)),
    cell2(Col, DownRow, safe),
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
    Action = "HitWall".

next_action(Action):-
    cell2(Col, Row, agent),
    cell2(Col, Row, gold),
    Action = "TakeGold".

next_action(Action):-
    cell2(Col, Row, agent),
    kill_wumpus_right(Col, Row),
    Action = "ShotRight".

next_action(Action):-
    cell2(Col, Row, agent),
    kill_wumpus_left(Col, Row),
    Action = "ShotLeft".

next_action(Action):-
    cell2(Col, Row, agent),
    kill_wumpus_up(Col, Row),
    Action = "ShotUp".

next_action(Action):-
    cell2(Col, Row, agent),
    kill_wumpus_down(Col, Row),
    Action = "ShotDown".

next_action(Action):-
    Action = "MoveNextCell".

kill_wumpus_right(Col, Row):-
    cell2(Col, Row, wumpusyes).

kill_wumpus_right(Col, Row):-
    in_limits(Col, Row),
    RightCol is Col+1,
    kill_wumpus_right(RightCol, Row).

kill_wumpus_left(Col, Row):-
    cell2(Col, Row, wumpusyes).

kill_wumpus_left(Col, Row):-
    in_limits(Col, Row),
    LeftCol is Col-1,
    kill_wumpus_left(LeftCol, Row).

kill_wumpus_up(Col, Row):-
    cell2(Col, Row, wumpusyes).

kill_wumpus_up(Col, Row):-
    in_limits(Col, Row),
    UpRow is Row+1,
    kill_wumpus_up(Col, UpRow).

kill_wumpus_down(Col, Row):-
    cell2(Col, Row, wumpusyes).

kill_wumpus_down(Col, Row):-
    in_limits(Col, Row),
    Down is Row-1,
    kill_wumpus_down(Col, Down).

%%%%%%%%%% GAME RULES %%%%%%%%%%
in_limits(Col, Row) :-
    grid_coord(MinCol, MinRow, MaxCol, MaxRow),
    numlist(MinCol, MaxCol, ListCol),
    numlist(MinRow, MaxRow, ListRow),
    member(Col, ListCol),
    member(Row, ListRow).

cell2(Col, Row, safe):-
    cell2(Col, Row, wall).

% cell2(Col, Row, safe):-
%     cell2(Col, Row, wumpusDead).

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

cell2(Col, Row, safe2):-
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    (
        \+(cell2(Col, Row, wumpus));
        \+(cell2(RightCol, Row, stench));
        \+(cell2(LeftCol, Row, stench));
        \+(cell2(Col, UpRow, stench));
        \+(cell2(Col, DownRow, stench))
    ),
    (
        \+(cell2(Col, Row, pit));
        \+(cell2(RightCol, Row, breeze));
        \+(cell2(LeftCol, Row, breeze));
        \+(cell2(Col, UpRow, breeze));
        \+(cell2(Col, DownRow, breeze))
    ).

%%% Define Stench & Wumpus attributes %%%
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
    \+(cell2(Col, Row, wumpusDead)),
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
    \+(cell2(Col, Row, wumpusDead)),
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
    \+(cell2(Col, Row, wumpusDead)),
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
    \+(cell2(Col, Row, wumpusDead)),
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
    \+(cell2(Col, Row, wumpusDead)),
    in_limits(Col, Row),
    cell2(Col, Row, wumpusyes),
    tnot(cell2(Col, Row, wumpusno)).

cell2(Col, Row, wumpus):-
    \+(cell2(Col, Row, wumpusDead)),
    in_limits(Col, Row),
    tnot(cell2(Col, Row, wumpusyes)),
    tnot(cell2(Col, Row, wumpusno)),
    tnot(cell2(Col, Row, wumpus)).

cell2(Col, Row, wumpus):-
    \+(cell2(Col, Row, wumpusDead)),
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
    in_limits(Col, Row),
    cell2(Col, Row, breezeyes),
    tnot(cell2(Col, Row, breezeno)).

cell2(Col, Row, breeze):-
    in_limits(Col, Row),
    tnot(cell2(Col, Row, breezeyes)),
    tnot(cell2(Col, Row, breezeno)),
    tnot(cell2(Col, Row, breeze)).

cell2(Col, Row, breeze):-
    in_limits(Col, Row),
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