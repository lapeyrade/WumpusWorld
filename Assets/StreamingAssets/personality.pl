:- module(personality, [personality/2]).

:- multifile(characteristic:characteristic/3).

% :- dynamic([determinist/1, stochastic/1, hunter/1, pacifist/1, explorer/1, greedy/1, nonmaterialistic/1]).

%%%%% ONTOLOGY PERSONALITY %%%%%
% PERSONALITY: DETERMINIST, STOCHASTIC, HUNTER, PACIFIST, EXPLORER, GREEDY, NONMATERIALIST

determinist(X):-
    characteristic:characteristic(X, intelligence, Value), Value >= 5.

stochastic(X):-
    characteristic:characteristic(X, intelligence, Value), Value < 5.

hunter(X):-
    characteristic:characteristic(X, strength, Value), Value >= 7,
    characteristic:characteristic(X, dexterity, Value2), Value2 >= 7.

pacifist(X):-
    characteristic:characteristic(X, intelligence, Value), Value > 7,
    characteristic:characteristic(X, strength, Value2), Value2 < 4.

explorer(X):-
    characteristic:characteristic(X, intelligence, Value), Value >= 7,
    characteristic:characteristic(X, dexterity, Value2), Value2 >= 7.

greedy(X):-
    characteristic:characteristic(X, intelligence, Value), Value =< 3.

nonmaterialistic(X):-
    characteristic:characteristic(X, intelligence, Value), Value > 7.


personality_(X) :- determinist(X).
personality_(X) :- stochastic(X).
personality_(X) :- hunter(X).
personality_(X) :- pacifist(X).
personality_(X) :- explorer(X).
personality_(X) :- greedy(X).
personality_(X) :- nonmaterialistic(X).

% Query PERSONALITY
personality(X, Personality):-
    clause(personality_(X), Pers),
    call(Pers),
    Pers =.. [Personality, _].