:- module(situation, [situation/2, cell/2]).

:- multifile [wellfs:is_false/1, wellfs:is_true/1, wellfs:is_undefined/1, alignment:alignment/3].

:- table situation/2 as incremental.

:- dynamic([cell/2, situation/2], [incremental(true)]).

%%%%%%%%%% SITUATION ONTOLOGY %%%%%%%%%%

% Intermediate predicate for cell
situation(Element, [X, Y]):-
    cell(Element, [X, Y]).

% agent and human are the same thing for now
situation(agent, [X, Y]):-
    situation(human, [X, Y]).

% cell has wumpusno if visited and no wumpus
situation(wumpusno, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(visited, [X, Y]),
    wellfs:is_false(situation(wumpusyes, [X, Y])),
    wellfs:is_false(situation(wumpusdead, [X, Y])).

% cell has pitno if visited and no pit
situation(pitno, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(visited, [X, Y]),
    wellfs:is_false(situation(pityes, [X, Y])).

% cell has breezeno if visited and no breeze
situation(breezeno, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(visited, [X, Y]),
    wellfs:is_false(situation(breezeyes, [X, Y])).

% cell has stenchno if visited and no stench
situation(stenchno, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(visited, [X, Y]),
    wellfs:is_false(situation(stenchyes, [X, Y])).

% cell is safe because it contains a wall
situation(safe, [X, Y]):-
    situation(wall, [X, Y]).

% cell is safe because it contains a dead wumpus
situation(safe, [X, Y]):-
    situation(wumpusdead, [X, Y]).

% cell is safe because it contains no enemy of the agent
situation(safe, [X, Y]):-
    in_limits(agent, [X, Y]),
    RightX is X+1,
    LeftX is X-1,
    UpY is Y+1,
    DownY is Y-1,
    alignment:alignment(human, Id2, enemy),
    wellfs:is_false(situation(Id2, [X, Y])),
    (
        wellfs:is_false(situation(stench, [RightX, Y]));
        wellfs:is_false(situation(stench, [LeftX, Y]));
        wellfs:is_false(situation(stench, [X, UpY]));
        wellfs:is_false(situation(stench, [X, DownY]))
    ),
    (
        wellfs:is_false(situation(breeze, [RightX, Y]));
        wellfs:is_false(situation(breeze, [LeftX, Y]));
        wellfs:is_false(situation(breeze, [X, UpY]));
        wellfs:is_false(situation(breeze, [X, DownY]))
    ).

% cell content is undefined, agent doesnt know if there is an enemy 
situation(undefined, [X, Y]):-
    in_limits(agent, [X, Y]),
    (
        wellfs:is_undefined(situation(wumpus, [X, Y]));
        wellfs:is_undefined(situation(pit, [X, Y]))
    ).

% cell is dangerous, agent knows there is an enemy 
situation(danger, [X, Y]):-
    in_limits(agent, [X, Y]),
    (
        wellfs:is_true(situation(pit, [X, Y]));
        wellfs:is_true(situation(wumpus, [X, Y]))
    ).

% cell is not visited but known as safe
situation(unvisited_safe_cell, [X, Y]):-
    \+ situation(visited, [X, Y]),
    \+ situation(wall, [X, Y]),
    situation(safe, [X, Y]).


%%% Define Stench & Wumpus attributes %%%

% cell has stenchyes if right cell has a wumpus
situation(stenchyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
   	(
        situation(wumpusyes, [RightX, Y]);
        situation(wumpusdead, [RightX, Y])
    ).

% cell has stenchyes if left cell has a wumpus
situation(stenchyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    LeftX is X-1,
    (
        situation(wumpusyes, [LeftX, Y]);
        situation(wumpusdead, [LeftX, Y])
    ).

% cell has stenchyes if up cell has a wumpus
situation(stenchyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    UpY is Y+1,
    (
        situation(wumpusyes, [X, UpY]);
        situation(wumpusdead, [X, UpY])
    ).

% cell has stenchyes if down cell has a wumpus
situation(stenchyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    DownY is Y-1,
    (
        situation(wumpusyes, [X, DownY]);
        situation(wumpusdead, [X, DownY])
    ).

% cell has stenchno if no adjacent cell has a wumpus
situation(stenchno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
    LeftX is X-1,
    UpY is Y+1,
    DownY is Y-1,
	situation(wumpusno, [RightX, Y]),
	situation(wumpusno, [LeftX, Y]),
	situation(wumpusno, [X, UpY]),
	situation(wumpusno, [X, DownY]).

% cell has stench if it has stenchyes and TNOT stenchno
situation(stench, [X, Y]):-
    situation(stenchyes, [X, Y]),
    tnot(situation(stenchno, [X, Y])).

% cell has stench if it has TNOT(stenchyes, stenchno & stench)
situation(stench, [X, Y]):-
    tnot(situation(stenchyes, [X, Y])),
    tnot(situation(stenchno, [X, Y])),
    tnot(situation(stench, [X, Y])).

% cell has stench if it has stenchyes, stenchno and TNOT stench
situation(stench, [X, Y]):-
    situation(stenchyes, [X, Y]),
    situation(stenchno, [X, Y]),
    tnot(situation(stench, [X, Y])).

% cell has wumpusno if right cell has stenchno
situation(wumpusno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
    situation(stenchno, [RightX, Y]).

% cell has wumpusno if left cell has stenchno
situation(wumpusno, [X, Y]) :-
    in_limits(agent, [X, Y]), 
    LeftX is X-1,
    situation(stenchno, [LeftX, Y]).

% cell has wumpusno if up cell has stenchno
situation(wumpusno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    UpY is Y+1,
    situation(stenchno, [X, UpY]).

% cell has wumpusno if down cell has stenchno
situation(wumpusno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    DownY is Y-1,
    situation(stenchno, [X, DownY]).

% cell has wumpusyes if RIGHT cell has stench 
% and no adjacent cell has wumpus
situation(wumpusyes, [X, Y]) :-
    \+(situation(wumpusdead, [X, Y])),
    in_limits(agent, [X, Y]),
    RightX is X+1,
    FarRightX is X+2,
    UpY is Y+1,
    DownY is Y-1,
	situation(wumpusno, [FarRightX, Y]),
	situation(wumpusno, [RightX, UpY]),
	situation(wumpusno, [RightX, DownY]), 
    situation(stenchyes, [RightX, Y]).

% cell has wumpusyes if LEFT cell has stench 
% and no adjacent cell has wumpus
situation(wumpusyes, [X, Y]) :-
    \+(situation(wumpusdead, [X, Y])),
    in_limits(agent, [X, Y]),
    LeftX is X-1,
    FarLeftX is X-2,
    UpY is Y+1,
    DownY is Y-1,
	situation(wumpusno, [FarLeftX, Y]),
	situation(wumpusno, [LeftX, UpY]), 
	situation(wumpusno, [LeftX, DownY]), 
    situation(stenchyes, [LeftX, Y]).

% cell has wumpusyes if UP cell has stench 
% and no adjacent cell has wumpus
situation(wumpusyes, [X, Y]) :-
    \+(situation(wumpusdead, [X, Y])),
    in_limits(agent, [X, Y]),
    UpY is Y+1,
    FarUpY is Y+2,
    RightX is X+1,
    LeftX is X-1,
	situation(wumpusno, [X, FarUpY]),
	situation(wumpusno, [LeftX, UpY]),
	situation(wumpusno, [RightX, UpY]),
    situation(stenchyes, [X, UpY]).

% cell has wumpusyes if DOWN cell has stench 
% and no adjacent cell has wumpus
situation(wumpusyes, [X, Y]) :-
    \+(situation(wumpusdead, [X, Y])),
    in_limits(agent, [X, Y]),
    DownY is Y-1,
    FarDownY is Y-2,
    RightX is X+1,
    LeftX is X-1,
	situation(wumpusno, [X, FarDownY]),
	situation(wumpusno, [LeftX, DownY]),
	situation(wumpusno, [RightX, DownY]),
    situation(stenchyes, [X, DownY]).

% cell has wumpus if wumpusyes and TNOT wumpusno
situation(wumpus, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(wumpusyes, [X, Y]),
    tnot(situation(wumpusno, [X, Y])).

% cell has wumpus if it has TNOT(wumpusyes, wumpusno & wumpus)
situation(wumpus, [X, Y]):-
    in_limits(agent, [X, Y]),
    tnot(situation(wumpusyes, [X, Y])),
    tnot(situation(wumpusno, [X, Y])),
    tnot(situation(wumpus, [X, Y])).

% cell has wumpus if it has wumpusyes, wumpusno and TNOT wumpus
situation(wumpus, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(wumpusyes, [X, Y]),
    situation(wumpusno, [X, Y]),
    tnot(situation(wumpus, [X, Y])).


%%% Define Breeze & Pit attributes %%%

% cell has breezeyes if right cell has a pit
situation(breezeyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
   	situation(pityes, [RightX, Y]).

% cell has breezeyes if left cell has a pit
situation(breezeyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    LeftX is X-1,
   	situation(pityes, [LeftX, Y]).

% cell has breezeyes if up cell has a pit
situation(breezeyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    UpY is Y+1,
   	situation(pityes, [X, UpY]).

% cell has breezeyes if down cell has a pit
situation(breezeyes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    DownY is Y-1,
   	situation(pityes, [X, DownY]).

% cell has breezeno if no adjacent cell has a pit
situation(breezeno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
    LeftX is X-1,
    UpY is Y+1,
    DownY is Y-1,
	situation(pitno, [RightX, Y]),
	situation(pitno, [LeftX, Y]),
	situation(pitno, [X, UpY]),
	situation(pitno, [X, DownY]).

% cell has breeze if it has breezeyes and TNOT breezeno
situation(breeze, [X, Y]):-
    situation(breezeyes, [X, Y]),
    tnot(situation(breezeno, [X, Y])).

% cell has breeze if it has TNOT(breezeyes, breezeno & breeze
situation(breeze, [X, Y]):-
    tnot(situation(breezeyes, [X, Y])),
    tnot(situation(breezeno, [X, Y])),
    tnot(situation(breeze, [X, Y])).

% cell has breeze if it has breezeyes, breezeno and TNOT breeze
situation(breeze, [X, Y]):-
    situation(breezeyes, [X, Y]),
    situation(breezeno, [X, Y]),
    tnot(situation(breeze, [X, Y])).

% cell has pitno if right cell has breezeno
situation(pitno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
   	situation(breezeno, [RightX, Y]).

% cell has pitno if left cell has breezeno
situation(pitno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    LeftX is X-1,
   	situation(breezeno, [LeftX, Y]).

% cell has pitno if up cell has breezeno
situation(pitno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    TopY is Y+1,
   	situation(breezeno, [X, TopY]).

% cell has pitno if down cell has breezeno
situation(pitno, [X, Y]) :-
    in_limits(agent, [X, Y]),
    DownY is Y-1,
    situation(breezeno, [X, DownY]).

% cell has pityes if RIGHT cell has breeze 
% and no adjacent cell has breeze
situation(pityes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    RightX is X+1,
    FarRightX is X+2,
    UpY is Y+1,
    DownY is Y-1,
	situation(pitno, [FarRightX, Y]),
	situation(pitno, [RightX, UpY]),
	situation(pitno, [RightX, DownY]), 
    situation(breezeyes, [RightX, Y]).

% cell has pityes if LEFT cell has breeze 
% and no adjacent cell has breeze
situation(pityes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    LeftX is X-1,
    FarLeftX is X-2,
    UpY is Y+1,
    DownY is Y-1,
	situation(pitno, [FarLeftX, Y]),
	situation(pitno, [LeftX, UpY]), 
	situation(pitno, [LeftX, DownY]), 
    situation(breezeyes, [LeftX, Y]).

% cell has pityes if UP cell has breeze 
% and no adjacent cell has breeze
situation(pityes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    UpY is Y+1,
    FarUpY is Y+2,
    RightX is X+1,
    LeftX is X-1,
	situation(pitno, [X, FarUpY]),
	situation(pitno, [LeftX, UpY]),
	situation(pitno, [RightX, UpY]),
    situation(breezeyes, [X, UpY]).

% cell has pityes if DOWN cell has breeze 
% and no adjacent cell has breeze
situation(pityes, [X, Y]) :-
    in_limits(agent, [X, Y]),
    DownY is Y-1,
    FarDownY is Y-2,
    RightX is X+1,
    LeftX is X-1,
	situation(pitno, [X, FarDownY]),
	situation(pitno, [LeftX, DownY]),
	situation(pitno, [RightX, DownY]),
    situation(breezeyes, [X, DownY]).

% cell has pit if pityes and TNOT pitno
situation(pit, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(pityes, [X, Y]),
    tnot(situation(pitno, [X, Y])).

% cell has pit if it has TNOT(pityes, pitno & pit)
situation(pit, [X, Y]):-
    in_limits(agent, [X, Y]),
    tnot(situation(pityes, [X, Y])),
    tnot(situation(pitno, [X, Y])),
    tnot(situation(pit, [X, Y])).

% cell has pit if it has pityes, pitno and TNOT pit
situation(pit, [X, Y]):-
    in_limits(agent, [X, Y]),
    situation(pityes, [X, Y]),
    situation(pitno, [X, Y]),
    tnot(situation(pit, [X, Y])).

% Defines the limits of agent reasoning
in_limits(agent, [X, Y]) :-
    situation(agent, [XAgent, YAgent]),
    MinX is XAgent - 3, MaxX is XAgent + 3,
    MinY is YAgent - 3, MaxY is YAgent + 3,
    numlist(MinX, MaxX, ListX),
    numlist(MinY, MaxY, ListY),
    member(X, ListX),
    member(Y, ListY).