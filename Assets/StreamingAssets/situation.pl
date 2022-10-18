:- module(situation, [situation/2, cell/2]).

:- multifile [wellfs:is_false/1, wellfs:is_true/1,
            wellfs:is_undefined/1, alignment:alignment/3].

:- table situation/2 as incremental.

:- dynamic([cell/2], [incremental(true)]).

%%%%%%%%%% SITUATION ONTOLOGY %%%%%%%%%%

% Intermediate predicate for cell
situation([Col, Row], Element):- cell([Col, Row], Element).

% agent and human are the same thing for now
situation([Col, Row], agent) :- situation([Col, Row], human).

% cell has wumpusno if visited and no wumpus
situation([Col, Row], wumpusno):-
    in_limits([Col, Row], agent),
    situation([Col, Row], visited),
    wellfs:is_false(situation([Col, Row], wumpusyes)),
    wellfs:is_false(situation([Col, Row], wumpusdead)).

% cell has pitno if visited and no pit
situation([Col, Row], pitno):-
    in_limits([Col, Row], agent),
    situation([Col, Row], visited),
    wellfs:is_false(situation([Col, Row], pityes)).

% cell has breezeno if visited and no breeze
situation([Col, Row], breezeno):-
    in_limits([Col, Row], agent),
    situation([Col, Row], visited),
    wellfs:is_false(situation([Col, Row], breezeyes)).

% cell has stenchno if visited and no stench
situation([Col, Row], stenchno):-
    in_limits([Col, Row], agent),
    situation([Col, Row], visited),
    wellfs:is_false(situation([Col, Row], stenchyes)).

% cell is safe because it contains a wall
situation([Col, Row], safe):-
    situation([Col, Row], wall).

% cell is safe because it contains a dead wumpus
situation([Col, Row], safe):-
    situation([Col, Row], wumpusdead).

% cell is safe because it contains no enemy of the agent
situation([Col, Row], safe):-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
    alignment:alignment(human, Y, enemy),
    wellfs:is_false(situation([Col, Row], Y)),
    (
        wellfs:is_false(situation([RightCol, Row], stench));
        wellfs:is_false(situation([LeftCol, Row], stench));
        wellfs:is_false(situation([Col, UpRow], stench));
        wellfs:is_false(situation([Col, DownRow], stench))
    ),
    (
        wellfs:is_false(situation([RightCol, Row], breeze));
        wellfs:is_false(situation([LeftCol, Row], breeze));
        wellfs:is_false(situation([Col, UpRow], breeze));
        wellfs:is_false(situation([Col, DownRow], breeze))
    ).

% cell content is undefined, agent doesnt know if there is an enemy 
situation([Col, Row], undefined):-
    in_limits([Col, Row], agent),
    (
        wellfs:is_undefined(situation([Col, Row], wumpus));
        wellfs:is_undefined(situation([Col, Row], pit))
    ).

% cell is dangerous, agent knows there is an enemy 
situation([Col, Row], danger):-
    in_limits([Col, Row], agent),
    wellfs:is_true(situation([Col, Row], pit));
    wellfs:is_true(situation([Col, Row], wumpus)).

% cell is not visited but known as safe
situation([Col, Row], unvisited_safe_cell):-
    \+ cell:situation([Col, Row], visited),
    \+ cell:situation([Col, Row], wall),
    cell:situation([Col, Row], safe).


%%% Define Stench & Wumpus attributes %%%

% cell has stenchyes if right cell has a wumpus
situation([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
   	(
        situation([RightCol, Row], wumpusyes);
        situation([RightCol, Row], wumpusdead)
    ).

% cell has stenchyes if left cell has a wumpus
situation([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    (
        situation([LeftCol, Row], wumpusyes);
        situation([LeftCol, Row], wumpusdead)
    ).

% cell has stenchyes if up cell has a wumpus
situation([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    (
        situation([Col, UpRow], wumpusyes);
        situation([Col, UpRow], wumpusdead)
    ).

% cell has stenchyes if down cell has a wumpus
situation([Col, Row], stenchyes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    (
        situation([Col, DownRow], wumpusyes);
        situation([Col, DownRow], wumpusdead)
    ).

% cell has stenchno if no adjacent cell has a wumpus
situation([Col, Row], stenchno) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([RightCol, Row], wumpusno),
	situation([LeftCol, Row], wumpusno),
	situation([Col, UpRow], wumpusno),
	situation([Col, DownRow], wumpusno).

% cell has stench if it has stenchyes and TNOT stenchno
situation([Col, Row], stench):-
    situation([Col, Row], stenchyes),
    tnot(situation([Col, Row], stenchno)).

% cell has stench if it has TNOT(stenchyes, stenchno & stench)
situation([Col, Row], stench):-
    tnot(situation([Col, Row], stenchyes)),
    tnot(situation([Col, Row], stenchno)),
    tnot(situation([Col, Row], stench)).

% cell has stench if it has stenchyes, stenchno and TNOT stench
situation([Col, Row], stench):-
    situation([Col, Row], stenchyes),
    situation([Col, Row], stenchno),
    tnot(situation([Col, Row], stench)).

% cell has wumpusno if right cell has stenchno
situation([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    RigtCol is Col+1,
    situation([RigtCol, Row], stenchno).

% cell has wumpusno if left cell has stenchno
situation([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent), 
    LeftCol is Col-1,
    situation([LeftCol, Row], stenchno).

% cell has wumpusno if up cell has stenchno
situation([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    situation([Col, UpRow], stenchno).

% cell has wumpusno if down cell has stenchno
situation([Col, Row], wumpusno) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    situation([Col, DownRow], stenchno).

% cell has wumpusyes if RIGHT cell has stench 
% and no adjacent cell has wumpus
situation([Col, Row], wumpusyes) :-
    \+(situation([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([FarRightCol, Row], wumpusno),
	situation([RightCol, UpRow], wumpusno),
	situation([RightCol, DownRow], wumpusno), 
    situation([RightCol, Row], stenchyes).

% cell has wumpusyes if LEFT cell has stench 
% and no adjacent cell has wumpus
situation([Col, Row], wumpusyes) :-
    \+(situation([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([FarLeftCol, Row], wumpusno),
	situation([LeftCol, UpRow], wumpusno), 
	situation([LeftCol, DownRow], wumpusno), 
    situation([LeftCol, Row], stenchyes).

% cell has wumpusyes if UP cell has stench 
% and no adjacent cell has wumpus
situation([Col, Row], wumpusyes) :-
    \+(situation([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	situation([Col, FarUpRow], wumpusno),
	situation([LeftCol, UpRow], wumpusno),
	situation([RightCol, UpRow], wumpusno),
    situation([Col, UpRow], stenchyes).

% cell has wumpusyes if DOWN cell has stench 
% and no adjacent cell has wumpus
situation([Col, Row], wumpusyes) :-
    \+(situation([Col, Row], wumpusdead)),
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	situation([Col, FarDownRow], wumpusno),
	situation([LeftCol, DownRow], wumpusno),
	situation([RightCol, DownRow], wumpusno),
    situation([Col, DownRow], stenchyes).

% cell has wumpus if wumpusyes and TNOT wumpusno
situation([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    situation([Col, Row], wumpusyes),
    tnot(situation([Col, Row], wumpusno)).

% cell has wumpus if it has TNOT(wumpusyes, wumpusno & wumpus)
situation([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    tnot(situation([Col, Row], wumpusyes)),
    tnot(situation([Col, Row], wumpusno)),
    tnot(situation([Col, Row], wumpus)).

% cell has wumpus if it has wumpusyes, wumpusno and TNOT wumpus
situation([Col, Row], wumpus):-
    in_limits([Col, Row], agent),
    situation([Col, Row], wumpusyes),
    situation([Col, Row], wumpusno),
    tnot(situation([Col, Row], wumpus)).


%%% Define Breeze & Pit attributes %%%

% cell has breezeyes if right cell has a pit
situation([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
   	situation([RightCol, Row], pityes).

% cell has breezeyes if left cell has a pit
situation([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
   	situation([LeftCol, Row], pityes).

% cell has breezeyes if up cell has a pit
situation([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
   	situation([Col, UpRow], pityes).

% cell has breezeyes if down cell has a pit
situation([Col, Row], breezeyes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
   	situation([Col, DownRow], pityes).

% cell has breezeno if no adjacent cell has a pit
situation([Col, Row], breezeno) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    LeftCol is Col-1,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([RightCol, Row], pitno),
	situation([LeftCol, Row], pitno),
	situation([Col, UpRow], pitno),
	situation([Col, DownRow], pitno).

% cell has breeze if it has breezeyes and TNOT breezeno
situation([Col, Row], breeze):-
    situation([Col, Row], breezeyes),
    tnot(situation([Col, Row], breezeno)).

% cell has breeze if it has TNOT(breezeyes, breezeno & breeze)
situation([Col, Row], breeze):-
    tnot(situation([Col, Row], breezeyes)),
    tnot(situation([Col, Row], breezeno)),
    tnot(situation([Col, Row], breeze)).

% cell has breeze if it has breezeyes, breezeno and TNOT breeze
situation([Col, Row], breeze):-
    situation([Col, Row], breezeyes),
    situation([Col, Row], breezeno),
    tnot(situation([Col, Row], breeze)).

% cell has pitno if right cell has breezeno
situation([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewCol is Col+1,
   	situation([NewCol, Row], breezeno).

% cell has pitno if left cell has breezeno
situation([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewCol is Col-1,
   	situation([NewCol, Row], breezeno).

% cell has pitno if up cell has breezeno
situation([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewRow is Row+1,
   	situation([Col, NewRow], breezeno).

% cell has pitno if down cell has breezeno
situation([Col, Row], pitno) :-
    in_limits([Col, Row], agent),
    NewRow is Row-1,
    situation([Col, NewRow], breezeno).

% cell has pityes if RIGHT cell has stench 
% and no adjacent cell has breeze
situation([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    RightCol is Col+1,
    FarRightCol is Col+2,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([FarRightCol, Row], pitno),
	situation([RightCol, UpRow], pitno),
	situation([RightCol, DownRow], pitno), 
    situation([RightCol, Row], breezeyes).

% cell has pityes if LEFT cell has stench 
% and no adjacent cell has breeze
situation([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    LeftCol is Col-1,
    FarLeftCol is Col-2,
    UpRow is Row+1,
    DownRow is Row-1,
	situation([FarLeftCol, Row], pitno),
	situation([LeftCol, UpRow], pitno), 
	situation([LeftCol, DownRow], pitno), 
    situation([LeftCol, Row], breezeyes).

% cell has pityes if UP cell has stench 
% and no adjacent cell has breeze
situation([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    UpRow is Row+1,
    FarUpRow is Row+2,
    RightCol is Col+1,
    LeftCol is Col-1,
	situation([Col, FarUpRow], pitno),
	situation([LeftCol, UpRow], pitno),
	situation([RightCol, UpRow], pitno),
    situation([Col, UpRow], breezeyes).

% cell has pityes if DOWN cell has stench 
% and no adjacent cell has breeze
situation([Col, Row], pityes) :-
    in_limits([Col, Row], agent),
    DownRow is Row-1,
    FarDownRow is Row-2,
    RightCol is Col+1,
    LeftCol is Col-1,
	situation([Col, FarDownRow], pitno),
	situation([LeftCol, DownRow], pitno),
	situation([RightCol, DownRow], pitno),
    situation([Col, DownRow], breezeyes).

% cell has pit if pityes and TNOT pitno
situation([Col, Row], pit):-
    in_limits([Col, Row], agent),
    situation([Col, Row], pityes),
    tnot(situation([Col, Row], pitno)).

% cell has pit if it has TNOT(pityes, pitno & pit)
situation([Col, Row], pit):-
    in_limits([Col, Row], agent),
    tnot(situation([Col, Row], pityes)),
    tnot(situation([Col, Row], pitno)),
    tnot(situation([Col, Row], pit)).

% cell has pit if it has pityes, pitno and TNOT pit
situation([Col, Row], pit):-
    in_limits([Col, Row], agent),
    situation([Col, Row], pityes),
    situation([Col, Row], pitno),
    tnot(situation([Col, Row], pit)).

% Defines the limits of agent reasoning
in_limits([Col, Row], X) :-
    situation([ColAgent, RowAgent], X),
    MinCol is ColAgent - 3, MaxCol is ColAgent + 3,
    MinRow is RowAgent - 3, MaxRow is RowAgent + 3,
    numlist(MinCol, MaxCol, ListCol),
    numlist(MinRow, MaxRow, ListRow),
    member(Col, ListCol),
    member(Row, ListRow).