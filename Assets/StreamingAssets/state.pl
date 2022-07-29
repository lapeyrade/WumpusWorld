:- module(state, [state/2]).

:- multifile [location:location/3, alignment:alignment/3].

%%%%%%%%%% STATE ONTOLOGY %%%%%%%%%%
% STATE: SAFETY, DANGER

% X is in a safe cell
safety(X):-
    location:location(X, safe, same_cell).

% X is in an ajacent cell to an enemy
danger(X):-
    location:location(X, Y, adjacent_cell),
    alignment:alignment(X, Y, enemy).

state_(X) :- danger(X).
state_(X) :- safety(X).

% QUERY State
state(X, State):-
    clause(state_(X), S),
    call(S),
    S =.. [State, _].