%% Objective Ontology - Defines the goals and motivations that drive agent behavior

%% Dynamic and incremental tabling declarations for objective predicates
%% Ensures efficient querying and updates of the objective knowledge base
:- table (objective/1, success/1, healthiness/1, unconstrained/1, explore/1, wealth/1,
    fight/1, abstinence/1, safety/1) as (incremental, dynamic).

%% Objective Hierarchy
%% Main categories of objectives that agents can pursue:
%% - success: achievement-oriented goals (wealth, combat)
%% - healthiness: survival and well-being goals
%% - unconstrained: freedom of movement goals
%% - explore: discovery and exploration goals
objective(X) :- success(X).
objective(X) :- healthiness(X).
objective(X) :- unconstrained(X).
objective(X) :- explore(X).

%% Success Objectives
%% - wealth: accumulating valuable items
%% - fight: engaging in combat
success(X) :- wealth(X).
success(X) :- fight(X).

%% Healthiness Objectives
%% - abstinence: avoiding unnecessary items/actions
%% - safety: avoiding dangerous situations
healthiness(X) :- abstinence(X).
healthiness(X) :- safety(X).