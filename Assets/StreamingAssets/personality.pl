:- module(personality, [personality/2]).

%%%%% ONTOLOGY PERSONALITY %%%%%

% PERSONALITY: DETERMINIST, STOCHASTIC, HUNTER, PACIFIST, EXPLORER, GREEDY
personality_(X) :- determinist(X).
personality_(X) :- stochastic(X).
personality_(X) :- hunter(X).
personality_(X) :- pacifist(X).
personality_(X) :- explorer(X).
personality_(X) :- greedy(X).

% Query PERSONALITY
personality(X, Personality):-
    clause(personality_(X), Pers),
    call(Pers),
    Pers =.. [Personality, _].