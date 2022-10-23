:- module(action, [action/2]).

:- multifile [wellfs:is_true/1, type:type/2, location:location/3, alignment:alignment/3, state:state/2, objective:objective/2, situation:situation/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: HIT_WALL, pickup_gold, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

% agent hits the wall so he cannot enter the cell
bump_wall(Id):-
    location:location(Id, wall, same_cell).

% agent picks gold
pickup_gold(Id):-
    location:location(Id, gold, same_cell),
    objective:objective(Id, find_gold).

shoot(Id1, Id2):-
    \+ objective:objective(Id1, avoid_killing),
    objective:objective(Id1, kill_wumpus);
    state:state(Id1, can_shoot_arrow),
    alignment:alignment(Id1, Id2, enemy),
    type:type(Id2, wumpus).

% shoot an arrow to the right
shoot_right(Id1):-
    shoot(Id1, Id2),
    wellfs:is_true(location(Id1, Id2, left_col)).

% shoot an arrow to the left
shoot_left(Id1):-
    shoot(Id1, Id2),
    wellfs:is_true(location(Id1, Id2, right_col)).

% shoot an arrow upward
shoot_up(Id1):-
    shoot(Id1, Id2),
    wellfs:is_true(location(Id1, Id2, down_row)).

% shoot an arrow downward
shoot_down(Id1):-
    shoot(Id1, Id2),
    wellfs:is_true(location(Id1, Id2, up_row)).

explore(Id):- objective:objective(Id, explore_cave).

action_(Id) :- bump_wall(Id).
action_(Id) :- pickup_gold(Id).
action_(Id) :- shoot_right(Id).
action_(Id) :- shoot_left(Id).
action_(Id) :- shoot_up(Id).
action_(Id) :- shoot_down(Id).
action_(Id) :- explore(Id).

% QUERY Action
action(Id, Action):-
    clause(action_(Id), A),
    call(A),
    A =.. [Action,_].


%%%% V2 %%%%%
% action(Id):- shoot_action(X).
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
