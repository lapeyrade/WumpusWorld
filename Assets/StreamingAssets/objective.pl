:- module(objective, [objective/2]).

:- multifile [personality:personality/2].

%%%%% ONTOLOGY OBJECTIVE %%%%%
% OBJECTIVES: FIND_GOLD, KILL_WUMPUS, AVOID_KILLING, EXPLORE_CAVE, DETERMINISTIC/STOCHASTIC_EXPLORATION

hunt_wumpus(Id):-
    personality:personality(Id, hunter),
    \+ personality:personality(Id, pacifist).

kill_wumpus(Id):-
    (
        personality:personality(Id, killer);
        personality:personality(Id, hunter)
    ),
    \+ personality:personality(Id, pacifist).

find_gold(Id):-
    personality:personality(Id, greedy),
    \+ personality:personality(Id, nonmaterialistic).

avoid_killing(Id):-
    personality:personality(Id, pacifist).

explore_cave(Id):-
    personality:personality(Id, explorer).

determinist_exploration(Id):-
    personality:personality(Id, determinist).

stochastic_exploration(Id):-
    personality:personality(Id, stochastic).

objective_(Id):- hunt_wumpus(Id).
objective_(Id):- find_gold(Id).
objective_(Id):- avoid_killing(Id).
objective_(Id):- explore_cave(Id).
objective_(Id):- determinist_exploration(Id).
objective_(Id):- stochastic_exploration(Id).

% Query OBJECTIVE
objective(Id, Objective):-
    clause(objective_(Id), Obj),
    call(Obj),
    Obj =.. [Objective, _].