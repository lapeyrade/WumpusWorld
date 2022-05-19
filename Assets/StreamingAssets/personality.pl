:- module(personality, [personality/2]).

%%%%% ONTOLOGY PERSONALITY %%%%%

% PERSONALITY: DETERMINIST, STOCHASTIC, HUNTER, PACIFIST
% determinist(dog) :- cell2(_, _, dog).
% stochastic(dog) :- cell2(_, _, dog).
% pacifist(dog) :- cell2(_, _, dog).
% hunter(dog) :- cell2(_, _, dog).

personality_(X) :- determinist(X).
personality_(X) :- stochastic(X).
personality_(X) :- hunter(X).
personality_(X) :- pacifist(X).

% Query PERSONALITY
personality(X, Personality):-
    clause(personality_(X), Pers),
    call(Pers),
    Pers =.. [Personality, _].