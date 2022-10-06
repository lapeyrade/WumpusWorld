:- module(cell, [cell2/2, cell/2]).

:- multifile [wellfs:is_false/1, wellfs:is_true/1,
            wellfs:is_undefined/1, alignment:alignment/3].

:- table cell2/2 as incremental.

:- dynamic([cell/2], [incremental(true)]).

%%%%%%%%%% CELL ONTOLOGY %%%%%%%%%%

% Intermediate predicate for cell
cell2([Col, Row], Element):- cell([Col, Row], Element).

% agent and human are the same thing for now
cell2([Col, Row], agent) :- cell2([Col, Row], human).

% cell has wumpusno if visited and no wumpus
cell2([Col, Row], wumpusno):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], visited),
    wellfs:is_false(cell2([Col, Row], wumpusyes)),
    wellfs:is_false(cell2([Col, Row], wumpusdead)).

% cell has pitno if visited and no pit
cell2([Col, Row], pitno):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], visited),
    wellfs:is_false(cell2([Col, Row], pityes)).

% cell has breezeno if visited and no breeze
cell2([Col, Row], breezeno):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], visited),
    wellfs:is_false(cell2([Col, Row], breezeyes)).

% cell has stenchno if visited and no stench
cell2([Col, Row], stenchno):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], visited),
    wellfs:is_false(cell2([Col, Row], stenchyes)).

% cell is safe because it contains a wall
cell2([Col, Row], safe):-
    cell2([Col, Row], wall).

% cell is safe because it contains a dead wumpus
cell2([Col, Row], safe):-
    cell2([Col, Row], wumpusdead).

% cell is safe because it contains no enemy of the agent
cell2([Col, Row], safe):-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    alignment:alignment(human, Y, enemy),
    wellfs:is_false(cell2([Col, Row], Y)),
    (
        wellfs:is_false(cell2([RightCol, Row], stench));
        wellfs:is_false(cell2([LeftCol, Row], stench));
        wellfs:is_false(cell2([Col, UpRow], stench));
        wellfs:is_false(cell2([Col, DownRow], stench))
    ),
    (
        wellfs:is_false(cell2([RightCol, Row], breeze));
        wellfs:is_false(cell2([LeftCol, Row], breeze));
        wellfs:is_false(cell2([Col, UpRow], breeze));
        wellfs:is_false(cell2([Col, DownRow], breeze))
    ).

% cell content is undefined, agent doesnt know if there is an enemy 
cell2([Col, Row], undefined):-
    in_limits([Col, Row], agent),
    wellfs:is_undefined(cell2([Col, Row], wumpus)),
    wellfs:is_undefined(cell2([Col, Row], pit)).

% cell is dangerous, agent knows there is an enemy 
cell2([Col, Row], danger):-
    in_limits([Col, Row], agent),
    wellfs:is_true(cell2([Col, Row], pit));
    wellfs:is_true(cell2([Col, Row], wumpus)).

% cell is not visited but known as safe
cell2([Col, Row], unvisited_safe_cell):-
    \+ cell:cell2([Col, Row], visited),
    \+ cell:cell2([Col, Row], wall),
    cell:cell2([Col, Row], safe).


%%% Define Stench & Wumpus attributes %%%

% cell has stenchyes if right cell has a wumpus
cell2([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
   	(
        cell2([RightCol, Row], wumpusyes);
        cell2([RightCol, Row], wumpusdead)
    ).

% cell has stenchyes if left cell has a wumpus
cell2([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    (
        cell2([LeftCol, Row], wumpusyes);
        cell2([LeftCol, Row], wumpusdead)
    ).

% cell has stenchyes if up cell has a wumpus
cell2([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    (
        cell2([Col, UpRow], wumpusyes);
        cell2([Col, UpRow], wumpusdead)
    ).

% cell has stenchyes if down cell has a wumpus
cell2([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    (
        cell2([Col, DownRow], wumpusyes);
        cell2([Col, DownRow], wumpusdead)
    ).

% cell has stenchno if no adjacent cell has a wumpus
cell2([Col, Row], stenchno) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([RightCol, Row], wumpusno),
	cell2([LeftCol, Row], wumpusno),
	cell2([Col, UpRow], wumpusno),
	cell2([Col, DownRow], wumpusno).

% cell has stench if it has stenchyes and TNOT stenchno
cell2([Col, Row], stench):-
    cell2([Col, Row], stenchyes),
    tnot(cell2([Col, Row], stenchno)).

% cell has stench if it has TNOT(stenchyes, stenchno & stench)
cell2([Col, Row], stench):-
    tnot(cell2([Col, Row], stenchyes)),
    tnot(cell2([Col, Row], stenchno)),
    tnot(cell2([Col, Row], stench)).

% cell has stench if it has stenchyes, stenchno and TNOT stench
cell2([Col, Row], stench):-
    cell2([Col, Row], stenchyes),
    cell2([Col, Row], stenchno),
    tnot(cell2([Col, Row], stench)).

% cell has wumpusno if right cell has stenchno
cell2([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    RigtCol is Col+1,
    cell2([RigtCol, Row], stenchno).

% cell has wumpusno if left cell has stenchno
cell2([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent), 
    LeftCol is Col-1,
    cell2([LeftCol, Row], stenchno).

% cell has wumpusno if up cell has stenchno
cell2([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    cell2([Col, UpRow], stenchno).

% cell has wumpusno if down cell has stenchno
cell2([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    cell2([Col, DownRow], stenchno).

% cell has wumpusyes if RIGHT cell has stench 
% and no adjacent cell has wumpus
cell2([Col, Row], wumpusyes) :-
    \+(cell2([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([FarRightCol, Row], wumpusno),
	cell2([RightCol, UpRow], wumpusno),
	cell2([RightCol, DownRow], wumpusno), 
    cell2([RightCol, Row], stenchyes).

% cell has wumpusyes if LEFT cell has stench 
% and no adjacent cell has wumpus
cell2([Col, Row], wumpusyes) :-
    \+(cell2([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([FarLeftCol, Row], wumpusno),
	cell2([LeftCol, UpRow], wumpusno), 
	cell2([LeftCol, DownRow], wumpusno), 
    cell2([LeftCol, Row], stenchyes).

% cell has wumpusyes if UP cell has stench 
% and no adjacent cell has wumpus
cell2([Col, Row], wumpusyes) :-
    \+(cell2([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2([Col, FarUpRow], wumpusno),
	cell2([LeftCol, UpRow], wumpusno),
	cell2([RightCol, UpRow], wumpusno),
    cell2([Col, UpRow], stenchyes).

% cell has wumpusyes if DOWN cell has stench 
% and no adjacent cell has wumpus
cell2([Col, Row], wumpusyes) :-
    \+(cell2([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2([Col, FarDownRow], wumpusno),
	cell2([LeftCol, DownRow], wumpusno),
	cell2([RightCol, DownRow], wumpusno),
    cell2([Col, DownRow], stenchyes).

% cell has wumpus if wumpusyes and TNOT wumpusno
cell2([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], wumpusyes),
    tnot(cell2([Col, Row], wumpusno)).

% cell has wumpus if it has TNOT(wumpusyes, wumpusno & wumpus)
cell2([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    tnot(cell2([Col, Row], wumpusyes)),
    tnot(cell2([Col, Row], wumpusno)),
    tnot(cell2([Col, Row], wumpus)).

% cell has wumpus if it has wumpusyes, wumpusno and TNOT wumpus
cell2([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], wumpusyes),
    cell2([Col, Row], wumpusno),
    tnot(cell2([Col, Row], wumpus)).


%%% Define Breeze & Pit attributes %%%

% cell has breezeyes if right cell has a pit
cell2([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
   	cell2([RightCol, Row], pityes).

% cell has breezeyes if left cell has a pit
cell2([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
   	cell2([LeftCol, Row], pityes).

% cell has breezeyes if up cell has a pit
cell2([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
   	cell2([Col, UpRow], pityes).

% cell has breezeyes if down cell has a pit
cell2([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
   	cell2([Col, DownRow], pityes).

% cell has breezeno if no adjacent cell has a pit
cell2([Col, Row], breezeno) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([RightCol, Row], pitno),
	cell2([LeftCol, Row], pitno),
	cell2([Col, UpRow], pitno),
	cell2([Col, DownRow], pitno).

% cell has breeze if it has breezeyes and TNOT breezeno
cell2([Col, Row], breeze):-
    cell2([Col, Row], breezeyes),
    tnot(cell2([Col, Row], breezeno)).

% cell has breeze if it has TNOT(breezeyes, breezeno & breeze)
cell2([Col, Row], breeze):-
    tnot(cell2([Col, Row], breezeyes)),
    tnot(cell2([Col, Row], breezeno)),
    tnot(cell2([Col, Row], breeze)).

% cell has breeze if it has breezeyes, breezeno and TNOT breeze
cell2([Col, Row], breeze):-
    cell2([Col, Row], breezeyes),
    cell2([Col, Row], breezeno),
    tnot(cell2([Col, Row], breeze)).

% cell has pitno if right cell has breezeno
cell2([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewCol is Col+1,
   	cell2([NewCol, Row], breezeno).

% cell has pitno if left cell has breezeno
cell2([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewCol is Col-1,
   	cell2([NewCol, Row], breezeno).

% cell has pitno if up cell has breezeno
cell2([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewRow is Row+1,
   	cell2([Col, NewRow], breezeno).

% cell has pitno if down cell has breezeno
cell2([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewRow is Row-1,
    cell2([Col, NewRow], breezeno).

% cell has pityes if RIGHT cell has stench 
% and no adjacent cell has breeze
cell2([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([FarRightCol, Row], pitno),
	cell2([RightCol, UpRow], pitno),
	cell2([RightCol, DownRow], pitno), 
    cell2([RightCol, Row], breezeyes).

% cell has pityes if LEFT cell has stench 
% and no adjacent cell has breeze
cell2([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	cell2([FarLeftCol, Row], pitno),
	cell2([LeftCol, UpRow], pitno), 
	cell2([LeftCol, DownRow], pitno), 
    cell2([LeftCol, Row], breezeyes).

% cell has pityes if UP cell has stench 
% and no adjacent cell has breeze
cell2([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2([Col, FarUpRow], pitno),
	cell2([LeftCol, UpRow], pitno),
	cell2([RightCol, UpRow], pitno),
    cell2([Col, UpRow], breezeyes).

% cell has pityes if DOWN cell has stench 
% and no adjacent cell has breeze
cell2([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	cell2([Col, FarDownRow], pitno),
	cell2([LeftCol, DownRow], pitno),
	cell2([RightCol, DownRow], pitno),
    cell2([Col, DownRow], breezeyes).

% cell has pit if pityes and TNOT pitno
cell2([Col, Row], pit):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], pityes),
    tnot(cell2([Col, Row], pitno)).

% cell has pit if it has TNOT(pityes, pitno & pit)
cell2([Col, Row], pit):-
    in_limits([Col, Row], agent),
    tnot(cell2([Col, Row], pityes)),
    tnot(cell2([Col, Row], pitno)),
    tnot(cell2([Col, Row], pit)).

% cell has pit if it has pityes, pitno and TNOT pit
cell2([Col, Row], pit):-
    in_limits([Col, Row], agent),
    cell2([Col, Row], pityes),
    cell2([Col, Row], pitno),
    tnot(cell2([Col, Row], pit)).

% Defines the limits of agent reasoning
in_limits([Col, Row], X) :-
    cell2([ColAgent, RowAgent], X),
    MinCol is ColAgent - 3, MaxCol is ColAgent + 3,
    MinRow is RowAgent - 3, MaxRow is RowAgent + 3,
    numlist(MinCol, MaxCol, ListCol),
    numlist(MinRow, MaxRow, ListRow),
    member(Col, ListCol),
    member(Row, ListRow).