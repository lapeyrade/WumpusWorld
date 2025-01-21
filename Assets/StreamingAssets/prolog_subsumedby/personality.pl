%% Personality Ontology - Defines character traits and behavioral tendencies
%% This file implements the personality hierarchy using Prolog rules

%% Dynamic and incremental tabling declarations for personality predicates
%% Ensures efficient querying and updates of the personality knowledge base
:- table (ambitious/1, disciplined/1, sensitive/1, cupid/1, 
    brave/1, ascetic/1, coward/1) as (incremental, dynamic).

%% Personality Hierarchy
%% Main personality types that influence agent behavior:
%% - ambitious: goal-oriented and achievement-focused traits
%% - disciplined: self-controlled and methodical traits
%% - sensitive: emotionally responsive traits
personality(X) :- ambitious(X).
personality(X) :- disciplined(X).
personality(X) :- sensitive(X).

%% Ambitious Personalities
%% Characters driven by achievement and conquest
ambitious(X) :- cupid(X).     % Motivated by acquisition and wealth
ambitious(X) :- brave(X).     % Motivated by combat and challenges

%% Disciplined Personalities
%% Characters with strong self-control
disciplined(X) :- ascetic(X). % Practices self-denial and restraint

%% Sensitive Personalities
%% Characters with heightened emotional responses
sensitive(X) :- coward(X).    % Prioritizes safety and avoidance of danger