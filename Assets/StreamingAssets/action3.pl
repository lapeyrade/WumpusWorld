:- multifile [cell:cell2/3, wellfs:is_true/1, type:type/2, location:location/3, alignment:alignment/3, state:state/2, objective:objective/2].

%%%%%%%%%% ACTION ONTOLOGY %%%%%%%%%%
% ACTION: BUMP_OBJECT, PICKUP_GOLD, SHOOT_ARROW, SHOOT_RIGHT/LEFT/UP/DOWM

% V1 
bump_object(X):- bump_obstacle(X).

bump_obstacle(X):- bump_wall(X).
bump_obstacle(X):- bump_rock(X).
bump_obstacle(X):- bump_locked_door(X).


% bump_wall(X):- location:location(X, Y, same_cell), type:type(Y, wall).
bump_wall(X):- location:location(X, wall, same_cell).
bump_rock(X):- location:location(X, rock, same_cell).
bump_locked_door(X):- location:location(X, locked_door, same_cell).

% V2
bump(X, Y):- bump_object(X, Y).
bump(X, Y):- bump_being(X, Y).

bump_object(X, Y):- location:location(X, Y, same_cell), type:type(Y, object),
                    bump_obstacle(X, Y).

bump_being(X, Y):- location:location(X, Y, same_cell), type:type(Y, being).

bump_obstacle(X, Y):- location:location(X, Y, same_cell), type:type(Y, obstacle),
                    (bump_wall(X); bump_rock(X); bump_locked_door(X)).


bump_wall(X):-  location:location(X, Y, same_cell), type:type(Y, wall).
bump_rock(X):-  location:location(X, Y, same_cell), type:type(Y, rock).
bump_locked_door(X):-  location:location(X, Y, same_cell), type:type(Y, locked_door).



bump(X, Y):- location:location(X, Z, same_cell), type:type(Y, Z).



shoot(X) :- shoot_right(X).
shoot(X) :- shoot_left(X).
shoot(X) :- shoot_up(X).
shoot(X) :- shoot_down(X).

% agent picks gold
pickup_gold(X):-
    location:location(X, gold, same_cell),
    objective:objective(X, find_gold).

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


action_(X) :- bump_object(X).
action_(X) :- pickup_gold(X).
action_(X) :- shoot_right(X).
action_(X) :- shoot_left(X).
action_(X) :- shoot_up(X).
action_(X) :- shoot_down(X).
action_(X) :- explore(X).

% QUERY Atom Action
action(X, AtomAction):-
    query_ontology(X, action_, AtomAction).

query_ontology(X, Atom, Atom):-
    PredicateAtom =.. [Atom, X],
    clause(PredicateAtom, A),
    call(A).

query_ontology(X, Atom, Atom2):-
    PredicateAtom =.. [Atom, X],
    clause(PredicateAtom, A),
    call(A),
    A =.. [Atom3, _],
    query_ontology(X, Atom3, Atom2).