:- module(objective, [objective/2]).

:- multifile [personality:personality/2].

%%%%% ONTOLOGY OBJECTIVE %%%%%
% OBJECTIVES: GOLD, KILL, AVOID, EXPLORE_ALL, DETERMINISTIC/STOCHASTIC_EXPLORATION

kill(X):-
    personality:personality(X, hunter).

gold(X):-
    personality:personality(X, greedy).

avoid(X):-
    personality:personality(X, pacifist).

explore_all(X):-
    personality:personality(X, explorer).

determinist_exploration(X):-
    personality:personality(X, determinist).

stochastic_exploration(X):-
    personality:personality(X, stochastic).

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