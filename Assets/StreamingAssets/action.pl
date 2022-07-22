:- module(action, [action/2]).

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, TAKE_GOLD, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

hit_wall(X):-
    cell2(Col, Row, X), cell2(Col, Row, wall).

take_gold(X):-
    cell2(Col, Row, X), cell2(Col, Row, gold).

can_shoot_arrow(X):-
    nb_arrow(Arrow), Arrow > 0,
    \+ objective(X, avoid).

% shoot an arrow to the left
shoot_right(X):-
    can_shoot_arrow(X),
    alignment(human, Y, enemy), type(Y, being),
    is_true(location(human, Y, left_col)).

% shoot an arrow to the left
shoot_left(X):-
    can_shoot_arrow(X),
    alignment(human, Y, enemy), type(Y, being),
    is_true(location(human, Y, right_col)).

% shoot an arrow upward
shoot_up(X):-
    can_shoot_arrow(X),
    alignment(human, Y, enemy), type(Y, being),
    is_true(location(human, Y, down_row)).

% shoot an arrow downward
shoot_down(X):-
    can_shoot_arrow(X),
    alignment(human, Y, enemy), type(Y, being),
    is_true(location(human, Y, up_row)).


action_(X) :- hit_wall(X).
action_(X) :- take_gold(X).
action_(X) :- shoot_right(X).
action_(X) :- shoot_left(X).
action_(X) :- shoot_up(X).
action_(X) :- shoot_down(X).

% QUERY Action
action(X, Action):-
    clause(action_(X), A),
    call(A),
    A =.. [Action,_].