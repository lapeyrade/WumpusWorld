% :- module(action, [action/1, bump_wall/1, pickup_gold/1,
                % shoot_right/1, shoot_left/1, shoot_up/1, shoot_down/1, explore/1]).

:- multifile [query_onto/3, is_true/1, element/2, location/3, alignment/3, state/2, objective/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, pickup_gold, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWN

% agent hits the wall so he cannot enter the cell
bump_wall(Id):-
    location(Id, wall, same_cell).

% agent picks gold
pickup_gold(Id):-
    location(Id, gold, same_cell),
    objective(Id, find_gold).

shoot(Id1, Id2):- 
    \+ objective(Id1, avoid_killing),
    objective(Id1, kill_wumpus),
    state(Id1, can_shoot_arrow),
    element(Id2, monster),
    alignment(Id1, Id2, enemy).

% shoot an arrow to the right
shoot_right(Id1):-
    shoot(Id1, Id2),
    is_true(location(Id1, Id2, left_col)).

% shoot an arrow to the left
shoot_left(Id1):-
    shoot(Id1, Id2),
    is_true(location(Id1, Id2, right_col)).

% shoot an arrow upward
shoot_up(Id1):-
    shoot(Id1, Id2),
    is_true(location(Id1, Id2, down_row)).

% shoot an arrow downward
shoot_down(Id1):-
    shoot(Id1, Id2),
    is_true(location(Id1, Id2, up_row)).

explore(Id):- objective(Id, explore_cave).

action(Id) :- bump_wall(Id).
action(Id) :- pickup_gold(Id).
action(Id) :- shoot_right(Id).
action(Id) :- shoot_left(Id).
action(Id) :- shoot_up(Id).
action(Id) :- shoot_down(Id).
action(Id) :- explore(Id).

% QUERY Action
action(Id, Action):-
    clause(action(Id), A),
    call(A),
    A =.. [Action,_].
