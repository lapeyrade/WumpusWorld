% :- module(alignment, [alignment/3]).

:- multifile [query_onto/3, element/2].

:- table ally/2, enemy/2.

%%%%% ONTOLOGY ALIGNMENT %%%%%
% ALIGNMENTS: ALLY, ENEMY, UNALIGNED

ally(Id1, Id2):-
    element(Id1, human), element(Id2, dog).

ally(Id1, Id2):-
    element(Id1, dog), element(Id2, human).

% ally of ally is ally
ally(Id1, Id2):-
    ally(Id1, Id3), ally(Id2, Id3).

enemy(Id1, Id2):-
    element(Id1, human), element(Id2, pit).

enemy(Id1, Id2):-
    element(Id1, pit), element(Id2, human).

enemy(Id1, Id2):-
    element(Id1, human), element(Id2, wumpus).

enemy(Id1, Id2):-
    element(Id1, wumpus), element(Id2, human).

% enemy of ally is enemy
enemy(Id1, Id2):-
    ally(Id1, Id3), enemy(Id3, Id2).

% ally of enemy is enemy
enemy(Id1, Id2):-
    enemy(Id1, Id3), ally(Id3, Id2).

% if not enemy nor ally then unaligned
unaligned(Id1, Id2):-
    element(Id1, _),
    element(Id2, _),
    Id1 \= Id2,
    \+ ally(Id1, Id2),
    \+ enemy(Id1, Id2).

alignment(Id1, Id2):- ally(Id1, Id2).
alignment(Id1, Id2):- enemy(Id1, Id2).
alignment(Id1, Id2):- unaligned(Id1, Id2).

alignment(Id1, Id2, Alignment):-
    clause(alignment(Id1, Id2), Align),
    call(Align),
    Align =.. [Alignment, _, _].