% :- module(state, [state/2, nb_gold/2, nb_arrow/2]).

:- dynamic([nb_gold/2, nb_arrow/2], [incremental(true)]).

:- multifile [query_onto/3, location/3, alignment/3, situation/2].

%%%%%%%%%% STATE ONTOLOGY %%%%%%%%%%
% STATE: SAFETY, DANGER, GOLD_FOUND, CANT_SHOOT_ARROW

% X can shoot arrow in any direction
can_shoot_arrow(Id):- nb_arrow(Id, Arrow), Arrow > 0.

% X has found at least one gold
gold_found(Id):- nb_gold(Id, NbGold), NbGold > 0.

% X is in a safe cell
safety(Id):-    
    situation(Id, [X, Y]),
    (
        situation(safe, [X, Y]);
        situation(visited, [X, Y])
    ).

% X is in an ajacent cell to an enemy
danger(Id1):-
    alignment(Id1, Id2, enemy),
    location(Id1, Id2, adjacent_cell).

state(Id) :- can_shoot_arrow(Id).
state(Id) :- gold_found(Id).
state(Id) :- safety(Id).
state(Id) :- danger(Id).

%%% QUERY STATE %%%
state(Id, State):-
    query_onto(Id, state, State).