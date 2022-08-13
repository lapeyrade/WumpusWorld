:- module(action2, [action2/2]).

:- multifile [cell:cell2/3, wellfs:is_true/1, type:type/2, location:location/3, alignment:alignment/3, state:state/2, objective:objective/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, pickup_gold, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

action(X):- shoot_action(X).
action(X):- hit_obstacle_action(X).
action(X):- pick_up_action(X).
action(X):- explore_action(X).

shoot_action(X):-
    shoot_right(X);
    shoot_left(X);
    shoot_up(X);
    shoot_down(X).

hit_obstacle_action(X):-
    hit_wall(X);
    hit_closed_door(X);
    hit_rock(X).

pick_up_action(X):-
    pickup_gold(X);
    pickup_arrow(X);
    pickup_weapon(X).

pick_up_weapon(X):-
    pickup_sword(X);
    pickup_bow(X).



% agent hits the wall so he cannot enter the cell
hit_wall(X):- location:location(X, wall, same_cell).

% agent picks gold
pickup_gold(X):-
    location:location(X, gold, same_cell),
    objective:objective(X, find_gold).

shoot(X, Y):-
    shoot_enemy(X, Y);
    shoot_ally(X, Y).

shoot_enemy(X, Y):-
    alignment:alignment(X, Y, enemy).

shoot_ally(X, Y):-
    alignment:alignment(X, Y, ally).

shoot(X, Y):-
    \+ objective:objective(X, avoid_killing),
    objective:objective(X, kill_wumpus),
    state:state(X, can_shoot_arrow),
    alignment:alignment(X, Y, enemy),
    type:type(Y, being).

% shoot an arrow to the left
shoot_right(X):-
    shoot(X, Y),
    wellfs:is_true(location(X, Y, left_col)).

% shoot an arrow to the left
shoot_left(X):-
    shoot(X, Y),
    wellfs:is_true(location(X, Y, right_col)).

% shoot an arrow upward
shoot_up(X):-
    shoot(X, Y),
    wellfs:is_true(location(X, Y, down_row)).

% shoot an arrow downward
shoot_down(X):-
    shoot(X, Y),
    wellfs:is_true(location(X, Y, up_row)).

explore(X):- objective:objective(X, explore_cave).

action_(X) :- hit_wall(X).
action_(X) :- pickup_gold(X).
action_(X) :- shoot_right(X).
action_(X) :- shoot_left(X).
action_(X) :- shoot_up(X).
action_(X) :- shoot_down(X).
action_(X) :- explore(X).

action_(X) :- action(X).

% QUERY Action
action(X, Action):-
    clause(action_(X), A),
    call(A),
    A =.. [Action,_].