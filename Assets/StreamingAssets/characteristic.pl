:- module(characteristic, [intelligence/2, strength/2, dexterity/2, characteristic/3]).

:- dynamic([intelligence/2, strength/2, dexterity/2], [incremental(true)]).

%%%%% ONTOLOGY PERSONALITY %%%%%

% PERSONALITY: INTELLIGENCE, STRENGTH, DEXTERITY
characteristic_(X, Value) :- intelligence(X, Value).
characteristic_(X, Value) :- strength(X, Value).
characteristic_(X, Value) :- dexterity(X, Value).

% Query PERSONALITY
characteristic(X, Characteristic, Value):-
    clause(characteristic_(X, Value), C),
    call(C),
    C =.. [Characteristic, _, _].