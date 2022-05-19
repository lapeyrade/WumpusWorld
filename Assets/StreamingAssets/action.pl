:- module(action, [action/2]).

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
hit_wall(X):-
    cell2(Col, Row, X),
    cell2(Col, Row, wall).

take_gold(X):-
    cell2(Col, Row, X),
    cell2(Col, Row, gold).

can_shot_arrow(X):-
    nb_arrow(Arrow), Arrow > 0,
    \+ personality(X, pacifist).

shot_right(X):-
    can_shot_arrow(X),
    cell2(Col, Row, X),
    is_true(cell2(ColWumpus, Row, wumpus)),
    ColWumpus > Col.

shot_left(X):-
    can_shot_arrow(X),
    cell2(Col, Row, X),
    is_true(cell2(ColWumpus, Row, wumpus)),
    ColWumpus < Col.

shot_up(X):-
    can_shot_arrow(X),
    cell2(Col, Row, X),
    is_true(cell2(Col, RowWumpus, wumpus)),
    RowWumpus > Row.

shot_down(X):-
    can_shot_arrow(X),
    cell2(Col, Row, X),
    is_true(cell2(Col, RowWumpus, wumpus)),
    RowWumpus < Row.

action_(X) :- hit_wall(X).
action_(X) :- take_gold(X).
action_(X) :- shot_right(X).
action_(X) :- shot_left(X).
action_(X) :- shot_up(X).
action_(X) :- shot_down(X).

% QUERY Action
action(X, Action):-
    clause(action_(X), A),
    call(A),
    A =.. [Action,_].





%%%%% ANCIENT VERSION %%%%%

% next_action(Action):-
%     cell2(Col, Row, agent),
%     cell2(Col, Row, wall),
%     Action = "HitWall".

% next_action(Action):-
%     cell2(Col, Row, agent),
%     cell2(Col, Row, gold),
%     Action = "TakeGold".

% next_action(Action):-
%     \+(personality(agent, pacifist)),
%     cell2(Col, Row, agent),
%     nb_arrow(Arrow), Arrow > 0,
%     kill_wumpus(Col, Row, Action).

% next_action(Action):-
%     Action = "MoveNextCell".

% kill_wumpus(Col, Row, Action):-
%     is_true(cell2(ColWumpus, Row, wumpus)),
%     ColWumpus > Col,
%     Action = "ShotRight".

% kill_wumpus(Col, Row, Action):-
%     is_true(cell2(ColWumpus, Row, wumpus)),
%     ColWumpus < Col,
%     Action = "ShotLeft".

% kill_wumpus(Col, Row, Action):-
%     is_true(cell2(Col, RowWumpus, wumpus)),
%     RowWumpus > Row,
%     Action = "ShotUp".

% kill_wumpus(Col, Row, Action):-
%     is_true(cell2(Col, RowWumpus, wumpus)),
%     RowWumpus < Row,
%     Action = "ShotDown".