% :- module(characteristic, [intelligence/2, strength/2, dexterity/2, characteristic/3]).

:- dynamic([intelligence/2, strength/2, dexterity/2], [incremental(true)]).

%%%%% ONTOLOGY PERSONALITY %%%%%

% PERSONALITY: INTELLIGENCE, STRENGTH, DEXTERITY
characteristic_(Id, Value) :- intelligence(Id, Value).
characteristic_(Id, Value) :- strength(Id, Value).
characteristic_(Id, Value) :- dexterity(Id, Value).

% Query PERSONALITY
characteristic(Id, Characteristic, Value):-
    clause(characteristic_(Id, Value), C),
    call(C), C =.. [Characteristic, _, _].