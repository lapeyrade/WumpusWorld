:- module(type, [type/2]).

:- table is_type/2.

%%%%% ONTOLOGY ELEMENT %%%%%
% TYPE: BEING, OBJECT 

is_type(human, being).
is_type(wumpus, monster).
is_type(monster, being).
is_type(dog, being).
is_type(pit, object).
is_type(wall, object).

is_type(X, Y):-
    is_type(X, Z),
    is_type(Z, Y).

being(X):-
    is_type(X, being).

object(X):-
    is_type(X, object).

type_(X) :- being(X).
type_(X) :- object(X).

% Query TYPE
type(X, Type):-
    clause(type_(X), Typ),
    call(Typ),
    Typ =.. [Type, _].