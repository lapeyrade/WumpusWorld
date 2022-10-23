:- module(state, [state/2, nb_gold/2, nb_arrow/2]).

:- dynamic([nb_gold/2, nb_arrow/2], [incremental(true)]).

:- multifile [location:location/3, alignment:alignment/3, situation:situation/2].

%%%%%%%%%% STATE ONTOLOGY %%%%%%%%%%
% STATE: SAFETY, DANGER, GOLD_FOUND, CANT_SHOOT_ARROW

% X can shoot arrow in any direction
can_shoot_arrow(Id):- nb_arrow(Id, Arrow), Arrow > 0.

% X has found at least one gold
gold_found(Id):- nb_gold(Id, NbGold), NbGold > 0.

% X is in a safe cell
safety(Id):-    
    situation:situation(Id, [X, Y]),
    (
        situation:situation(safe, [X, Y]);
        situation:situation(visited, [X, Y])
    ).

% X is in an ajacent cell to an enemy
danger(Id1):-
    location:location(Id1, Id2, adjacent_cell).
    alignment:alignment(Id1, Id2, enemy).

state_(Id) :- can_shoot_arrow(Id).
state_(Id) :- gold_found(Id).
state_(Id) :- safety(Id).
state_(Id) :- danger(Id).

% QUERY State
state(Id, State):-
    clause(state_(Id), S),
    call(S),
    S =.. [State, _].


% all golds are found by the agent
% all_golds_found(Id) :-
    % nb_gold(TotalGold), nb_gold_agent(X, AgentGold),
    % TotalGold == AgentGold.

% all wumpus are killed by the agent
% all_wumpus_killed(Id) :-
    % nb_wumpus(TotalWumpus), nb_wumpus_dead(X, WumpusDead),
    % TotalWumpus == WumpusDead.