:- module(type, [type/2]).

:- table type/2.

%%%%% ONTOLOGY TYPE %%%%%
% TYPE: BEING, OBJECT 

type(being, element).
type(object, element).

type(animal, being).
type(plant, being).
type(obstacle, object).
type(trap, object).

type(monster, animal).
type(beast, animal).
type(human, animal).
type(wall, obstacle).
type(rock, obstacle).
type(pit, trap).
type(cage, trap).

type(dog, beast).
type(deer, beast).
type(wumpus, monster).
type(dragon, monster).

% X is type of X, Z if type of Y then X is type of Y 
type(X, Y):- type(X, Z), type(Z, Y).

% X is type of itself
type(X, X):-  type(X, _).

%% Pas définitif
% type(w1, wumpus).
% type(w1(X), wumpus).

% type_(X) :- being(X).
% type_(X) :- object(X).

% % Query TYPE
% type(X, Type):-
%     clause(type_(X), T),
%     call(T),
%     T =.. [Type, _].


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


% wumpus(w(X, Y)):- cl2(X, Y, wumpus).

% being(X):- monster(X).
% monster(X):- wumpus(X).

% type_(X) :- being(X).
% % type_(X) :- object(X).

% Query TYPE
% type(X, Type):-
%     clause(type_(X), Typ),
%     call(Typ),
%     Typ =.. [Type, _].