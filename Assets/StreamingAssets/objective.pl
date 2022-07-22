:- module(objective, [objective/2]).

%%%%% ONTOLOGY OBJECTIVE %%%%%
% OBJECTIVES: GOLD, KILL, AVOID, EXPLORE_ALL, DETERMINISTIC/STOCHASTIC_EXPLORATION

kill(X):-
    personality(X, hunter).

gold(X):-
    personality(X, greedy).

avoid(X):-
    personality(X, pacifist).

explore_all(X):-
    personality(X, explorer).

determinist_exploration(X):-
    personality(X, determinist).

stochastic_exploration(X):-
    personality(X, stochastic).

objective_(X):- kill(X).
objective_(X):- gold(X).
objective_(X):- avoid(X).
objective_(X):- explore_all(X).
objective_(X):- determinist_exploration(X).
objective_(X):- stochastic_exploration(X).

% Query OBJECTIVE
objective(X, Objective):-
    clause(objective_(X), Obj),
    call(Obj),
    Obj =.. [Objective, _].