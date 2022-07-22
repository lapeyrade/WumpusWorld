:- module(action, [action/2]).

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, TAKE_GOLD, CAN_SHOT_ARROW, SHOT_RIGHT/LEFT/UP/DOWM

hit_wall(X):-
    cell2(Col, Row, X),
    cell2(Col, Row, wall).

take_gold(X):-
    cell2(Col, Row, X),
    cell2(Col, Row, gold).

can_shot_arrow(X):-
    nb_arrow(Arrow), Arrow > 0,
    \+ objective(X, avoid).

shot_right(X):-
    can_shot_arrow(X),
    alignment(human, Y, enemy),
    is_true(location(human, Y, left_col)),
    type(Y, being).
    
shot_left(X):-
    can_shot_arrow(X),
    alignment(human, Y, enemy),
    is_true(location(human, Y, right_col)),
    type(Y, being).

shot_up(X):-
    can_shot_arrow(X),
    alignment(human, Y, enemy),
    is_true(location(human, Y, down_row)),
    type(Y, being).
    
shot_down(X):-
    can_shot_arrow(X),
    alignment(human, Y, enemy),
    is_true(location(human, Y, up_row)),
    type(Y, being).

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