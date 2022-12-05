% :- module(personality, [personality/2]).

:- multifile([query_onto/3, characteristic/3]).

%%%%% ONTOLOGY PERSONALITY %%%%%
% PERSONALITY: DETERMINIST, STOCHASTIC, HUNTER, PACIFIST, EXPLORER, GREEDY, NONMATERIALISTIC

determinist(Id):-
    characteristic(Id, dexterity, Value),
    Value >= 5.

stochastic(Id):-
    characteristic(Id, dexterity, Value),
    Value < 5.

hunter(Id):-
    characteristic(Id, strength, Value),
    Value >= 7.

killer(Id):-
    characteristic(Id, strength, Value),
    Value > 4, Value < 7.

pacifist(Id):-
    characteristic(Id, strength, Value),
    Value =< 4.

explorer(Id):-
    characteristic(Id, intelligence, Value),
    Value > 4, Value < 7.

greedy(Id):-
    characteristic(Id, intelligence, Value),
    Value =< 4.

nonmaterialistic(Id):-
    characteristic(Id, intelligence, Value),
    Value >= 7.

personality(Id) :- determinist(Id).
personality(Id) :- stochastic(Id).
personality(Id) :- hunter(Id).
personality(Id) :- killer(Id).
personality(Id) :- pacifist(Id).
personality(Id) :- explorer(Id).
personality(Id) :- greedy(Id).
personality(Id) :- nonmaterialistic(Id).

% Query PERSONALITY
% personality(Id, Personality):-
%     clause(personality(Id), Pers),
%     call(Pers),
%     Pers =.. [Personality, _].

personality(Id, Personality):-
    query_onto(Id, personality, Personality).