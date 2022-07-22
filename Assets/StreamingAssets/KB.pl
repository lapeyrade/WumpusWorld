:- use_module([action, alignment, cell, location, move, objective, personality, type, wellfs]).

% Dynamic predicate in main file if not already defined in used modules
:- dynamic([nb_wumpus/1, nb_wumpus_dead/1,
    nb_arrow/1, nb_arrow_used/1, nb_gold/1,
    nb_gold_agent/1, grid_coord/4],
    [incremental(true)]).

% Personality 
:- dynamic([determinist/1, stochastic/1, hunter/1, pacifist/1, explorer/1, greedy/1]).