:- module(personality, [personality/2]).

:- multifile(characteristic:characteristic/3).

%%%%% ONTOLOGY PERSONALITY %%%%%
% PERSONALITY: DETERMINIST, STOCHASTIC, HUNTER, PACIFIST, EXPLORER, GREEDY, NONMATERIALIST

determinist(Id):-
    characteristic:characteristic(Id, dexterity, Value),
    Value >= 5.

stochastic(Id):-
    characteristic:characteristic(Id, dexterity, Value),
    Value < 5.

hunter(Id):-
    characteristic:characteristic(Id, strength, Value),
    Value >= 7.

killer(Id):-
    characteristic:characteristic(Id, strength, Value),
    Value > 4, Value < 7.

pacifist(Id):-
    characteristic:characteristic(Id, strength, Value),
    Value =< 4.

explorer(Id):-
    characteristic:characteristic(Id, intelligence, Value),
    Value > 4, Value < 7.

greedy(Id):-
    characteristic:characteristic(Id, intelligence, Value),
    Value =< 4.

nonmaterialistic(Id):-
    characteristic:characteristic(Id, intelligence, Value),
    Value >= 7.

personality_(Id) :- determinist(Id).
personality_(Id) :- stochastic(Id).
personality_(Id) :- hunter(Id).
personality_(Id) :- killer(Id).
personality_(Id) :- pacifist(Id).
personality_(Id) :- explorer(Id).
personality_(Id) :- greedy(Id).
personality_(Id) :- nonmaterialistic(Id).

% Query PERSONALITY
personality(Id, Personality):-
    clause(personality_(Id), Pers),
    call(Pers),
    Pers =.. [Personality, _].