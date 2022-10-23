:- module(alignment, [alignment/3]).

:- multifile [situation:situation/2].

:- table ally/2, enemy/2.

%%%%% ONTOLOGY ALIGNMENT %%%%%
% ALIGNMENTS: ALLY, ENEMY, UNALIGNED
% ally(human, human).
% ally(dog, dog).
ally(human, dog).
ally(dog, human).

% ally of ally is ally
ally(Id1, Id2):-
    ally(Id1, Id3),
    ally(Id2, Id3).

enemy(human, pit).
enemy(pit, human).

enemy(human, wumpus).
enemy(wumpus, human).

% enemy of ally is enemy
enemy(Id1, Id2):-
    ally(Id1, Id3),
    enemy(Id3, Id2).

% ally of enemy is enemy
enemy(Id1, Id2):-
    enemy(Id1, Id3),
    ally(Id3, Id2).

% if not enemy nor ally then unaligned
unaligned(Id1, Id2):-
    situation:situation(Id1, _),
    situation:situation(Id2, _),
    Id1 \= Id2, \+ ally(Id1, Id2), \+ enemy(Id1, Id2).

alignment_(Id1, Id2):- ally(Id1, Id2).
alignment_(Id1, Id2):- enemy(Id1, Id2).
% alignment_(Id1, Id2):- unaligned(Id1, Id2).

% QUERY ALIGNMENT
alignment(Id1, Id2, Alignment):-
    clause(alignment_(Id1, Id2), Align),
    call(Align),
    Align =.. [Alignment, _, _].