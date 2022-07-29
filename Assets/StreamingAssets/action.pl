:- module(action, [action/2, nb_arrow/1]).

:- dynamic([nb_arrow/1], [incremental(true)]).

:- multifile [cell:cell2/3, wellfs:is_true/1, type:type/2, location:location/3, alignment:alignment/3, objective:objective/2, desire:desire/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, TAKE_GOLD, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

% agent hits the wall so he cannot enter the cell
hit_wall(X):-
    location:location(X, wall, same_cell).

% agent picks gold
take_gold(X):-
    location:location(X, gold, same_cell),
    desire:desire(X, get_rich).

% agent can shoot arrow in any direction
can_shoot_arrow(human):-
    nb_arrow(Arrow), Arrow > 0,
    \+ objective:objective(human, avoid).

% shoot an arrow to the left
shoot_right(X):-
    desire:desire(X, kill_wumpus),
    can_shoot_arrow(X),
    alignment:alignment(X, Y, enemy),
    type:type(Y, being),
    wellfs:is_true(location(X, Y, left_col)).

% shoot an arrow to the left
shoot_left(X):-
    desire:desire(X, kill_wumpus),
    can_shoot_arrow(X),
    alignment:alignment(X, Y, enemy),
    type:type(Y, being),
    wellfs:is_true(location(X, Y, right_col)).

% shoot an arrow upward
shoot_up(X):-
    desire:desire(X, kill_wumpus),
    can_shoot_arrow(X),
    alignment:alignment(X, Y, enemy),
    type:type(Y, being),
    wellfs:is_true(location(X, Y, down_row)).

% shoot an arrow downward
shoot_down(X):-
    can_shoot_arrow(X),
    alignment:alignment(X, Y, enemy),
    type:type(Y, being),
    wellfs:is_true(location(X, Y, up_row)).

explore(X):-
    desire:desire(X, explore_cave).

action_(X) :- hit_wall(X).
action_(X) :- take_gold(X).
action_(X) :- shoot_right(X).
action_(X) :- shoot_left(X).
action_(X) :- shoot_up(X).
action_(X) :- shoot_down(X).
action_(X) :- explore(X).

% QUERY Action
action(X, Action):-
    clause(action_(X), A),
    call(A),
    A =.. [Action,_].