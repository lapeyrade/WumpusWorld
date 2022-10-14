:- module(objective, [objective/2]).

:- multifile [personality:personality/2].

%%%%% ONTOLOGY OBJECTIVE %%%%%
% OBJECTIVES: FIND_GOLD, KILL_WUMPUS, AVOID_KILLING, EXPLORE_CAVE, DETERMINISTIC/STOCHASTIC_EXPLORATION

hunt_wumpus(X):-
    personality:personality(X, hunter),
    \+ personality:personality(X, pacifist).

kill_wumpus(X):-
    (
        personality:personality(X, killer);
        personality:personality(X, hunter)
    ),
    \+ personality:personality(X, pacifist).

find_gold(X):-
    personality:personality(X, greedy),
    \+ personality:personality(X, nonmaterialistic).

avoid_killing(X):-
    personality:personality(X, pacifist).

explore_cave(X):-
    personality:personality(X, explorer).

determinist_exploration(X):-
    personality:personality(X, determinist).

stochastic_exploration(X):-
    personality:personality(X, stochastic).

objective_(X):- hunt_wumpus(X).
objective_(X):- find_gold(X).
objective_(X):- avoid_killing(X).
objective_(X):- explore_cave(X).
objective_(X):- determinist_exploration(X).
objective_(X):- stochastic_exploration(X).

% Query OBJECTIVE
objective(X, Objective):-
    clause(objective_(X), Obj),
    call(Obj),
    Obj =.. [Objective, _].