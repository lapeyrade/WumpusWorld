:- multifile [element/2].

%%%%% ONTOLOGY BELIEF %%%%%
ally(Id1, Id2):-
    element(Id1, human),
    element(Id2, dog).

enemy(Id1, Id2):-
    element(Id1, human),
    element(Id2, wumpus).

dangerous(Id):- element(Id, monster).
dangerous(Id):- element(Id, trap).
dangerous(Id):- element(Id, human).

harmless(Id):-
    element(Id, object),
    \+ element(Id, trap).

danger(Id1, Id2):-
    enemy(Id1, Id2),
    dangerous(Id1).