:- module(state, [state/2, nb_gold/2, nb_arrow/2]).

:- dynamic([nb_gold/2, nb_arrow/2], [incremental(true)]).

:- multifile [location:location/3, alignment:alignment/3].

%%%%%%%%%% STATE ONTOLOGY %%%%%%%%%%
% STATE: SAFETY, DANGER, GOLD_FOUND, CANT_SHOOT_ARROW

% X can shoot arrow in any direction
can_shoot_arrow(Id):- nb_arrow(Id, Arrow), Arrow > 0.

% X has found at least one gold
gold_found(Id):- nb_gold(Id, NbGold), NbGold > 0.

% X is in a safe cell
safety(X):- location:location(X, safe, same_cell).

% X is in an ajacent cell to an enemy
danger(X):-
    location:location(X, Y, adjacent_cell),
    alignment:alignment(X, Y, enemy).

state_(X) :- can_shoot_arrow(X).
state_(X) :- gold_found(X).
state_(X) :- danger(X).
state_(X) :- safety(X).

% QUERY State
state(X, State):-
    clause(state_(X), S),
    call(S),
    S =.. [State, _].


% all golds are found by the agent
% all_golds_found(X) :-
    % nb_gold(TotalGold), nb_gold_agent(X, AgentGold),
    % TotalGold == AgentGold.

% all wumpus are killed by the agent
% all_wumpus_killed(X) :-
    % nb_wumpus(TotalWumpus), nb_wumpus_dead(X, WumpusDead),
    % TotalWumpus == WumpusDead.