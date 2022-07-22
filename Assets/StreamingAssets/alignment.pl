:- module(alignment, [alignment/3]).

:- table enemy/2.

%%%%% ONTOLOGY ALIGNMENT %%%%%
% ALIGNMENTS: ALLY, ENEMY, UNALIGNED

ally(human, dog).
ally(dog, human).

enemy(human, wumpus).
enemy(wumpus, human).
enemy(human, pit).
enemy(pit, human).

% enemy of ally is enemy
enemy(X, Y):-
    ally(X, Z),
    enemy(Z, Y).

% ally of enemy is enemy
enemy(X, Y):-
    enemy(X, Z),
    ally(Z, Y).

% if not enemy nor ally then unaligned
unaligned(X, Y):-
    type(_, X), type(_, Y),
    X \= Y, \+ ally(X, Y), \+ enemy(X, Y).

alignment_(X, Y):- ally(X, Y).
alignment_(X, Y):- enemy(X, Y).
alignment_(X, Y):- unaligned(X, Y).

% QUERY ALIGNMENT
alignment(X, Y, Alignment):-
    clause(alignment_(X, Y), Align),
    call(Align),
    Align =.. [Alignment, _, _].