:- module(action, [action/2]).

:- multifile [wellfs:is_true/1, type:type/2, location:location/3, alignment:alignment/3, state:state/2, objective:objective/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, pickup_gold, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

% agent hits the wall so he cannot enter the cell
bump_wall(X):- location:location(X, wall, same_cell).
bump_rock(X):- location:location(X, rock, same_cell).
bump_locked_door(X):- location:location(X, locked_door, same_cell).

% agent picks gold
pickup_gold(X):-
    location:location(X, gold, same_cell),
    objective:objective(X, find_gold).

shoot(X, Y):-
    \+ objective:objective(X, avoid_killing),
    objective:objective(X, kill_wumpus);
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


action_(X) :- bump_locked_door(X).
action_(X) :- bump_rock(X).
action_(X) :- bump_wall(X).
action_(X) :- pickup_gold(X).
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


%%%% V2 %%%%%
% action(X):- shoot_action(X).
% action(X):- hit_obstacle_action(X).
% action(X):- pick_up_action(X).
% action(X):- explore_action(X).

% shoot_action(X):-
%     shoot_right(X);
%     shoot_left(X);
%     shoot_up(X);
%     shoot_down(X).

% hit_obstacle_action(X):-
%     hit_wall(X);
%     hit_closed_door(X);
%     hit_rock(X).

% pick_up_action(X):-
%     pickup_gold(X);
%     pickup_arrow(X);
%     pickup_weapon(X).

% pick_up_weapon(X):-
%     pickup_sword(X);
%     pickup_bow(X).

%%%% V3 %%%%%
% bump_object(X):- bump_obstacle(X).

% bump_obstacle(X):- bump_wall(X).
% bump_obstacle(X):- bump_rock(X).
% bump_obstacle(X):- bump_locked_door(X).


% % bump_wall(X):- location:location(X, Y, same_cell), type:type(Y, wall).
% bump_wall(X):- location:location(X, wall, same_cell).
% bump_rock(X):- location:location(X, rock, same_cell).
% bump_locked_door(X):- location:location(X, locked_door, same_cell).

% V4
% bump(X, Y):- bump_object(X, Y).
% bump(X, Y):- bump_being(X, Y).

% bump_object(X, Y):- location:location(X, Y, same_cell), type:type(Y, object),
%                     bump_obstacle(X, Y).

% bump_being(X, Y):- location:location(X, Y, same_cell), type:type(Y, being).

% bump_obstacle(X, Y):- location:location(X, Y, same_cell), type:type(Y, obstacle),
%                     (bump_wall(X); bump_rock(X); bump_locked_door(X)).


% bump_wall(X):-  location:location(X, Y, same_cell), type:type(Y, wall).
% bump_rock(X):-  location:location(X, Y, same_cell), type:type(Y, rock).
% bump_locked_door(X):-  location:location(X, Y, same_cell), type:type(Y, locked_door).

% bump(X, Y):- location:location(X, Z, same_cell), type:type(Y, Z).
