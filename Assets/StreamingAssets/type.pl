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


% being(X):- human(X).

% :- dynamic([wumpus/1], [incremental(true)]).

% human(h1).

% wumpus(w1).
% wumpus(w2).
% wumpus(w3).
% wumpus(w4).
% wumpus(w5).

% pit(p1).
% pit(p2).

% wumpus.
% pit.

% being :- wumpus.

% being(X) :- wumpus(X).


% wumpus(w(X, Y)):- cell2(X, Y, wumpus).

% being(X):- monster(X).
% monster(X):- wumpus(X).

% type_(X) :- being(X).
% % type_(X) :- object(X).

% Query TYPE
% type(X, Type):-
%     clause(type_(X), Typ),
%     call(Typ),
%     Typ =.. [Type, _].