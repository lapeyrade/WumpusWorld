%% Objective Ontology - Defines the goals and motivations that drive agent behavior
%% This file implements the objective hierarchy using Prolog rules

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
%% Goals related to achieving specific outcomes
success(X) :- wealth(X).      % Accumulating valuable items
success(X) :- fight(X).       % Engaging in combat

%% Healthiness Objectives
%% Goals related to maintaining agent well-being
healthiness(X) :- abstinence(X).  % Avoiding unnecessary items/actions
healthiness(X) :- safety(X).      % Avoiding dangerous situations