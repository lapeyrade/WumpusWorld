%% Core decision-making and behavior rules
%% This file implements the main reasoning system for agent behavior

%% Load required ontologies and knowledge bases
:-consult([onto, element, action, personality, objective, localization]).

%% Initialize classifications for core ontologies
:- classification(element,1), classification(objective,1), classification(personality,1), classification(action,1).

%% Dynamic and incremental tabling declarations for reasoning predicates
%% Ensures efficient querying and updates of the behavioral knowledge base
:- table (subsumes_or_equals/2, generalize_pair/4, has_personality_trait/2, desirable/2,
    motivation/2, satisfy/2, encline/3, genObjective/4, genAction/6) as (incremental, dynamic).

%% Dynamic declarations for personality traits and concept data
:- dynamic([has_personality_trait/2, data_concept/2], [incremental(true)]).

%% Decision Making System
%% Core predicates and rules for agent decision making

%% Subsumption Check
%% Determines if one concept subsumes or equals another
subsumes_or_equals(X, Y) :-
    X = Y;                        % Direct equality
    strictSubsumedBy(X, Y);       % Strict subsumption
    data_concept(X, Y).           % Data-based concept relationship

%% Generalization Pattern Helper
%% Helps find more general concepts that subsume specific instances
generalize_pair(X1, X2, SupX1, SupX2) :-
    subsumes_or_equals(X1, SupX1),
    subsumes_or_equals(X2, SupX2),
    (X1 \= SupX1; X2 \= SupX2).

%% Personality System
%% Rules for determining and inheriting personality traits
has_personality_trait(Entity, SupPerso):-
    has_personality_trait(SupEntity, Perso),
    generalize_pair(Entity, Perso, SupEntity, SupPerso).

%% Desire System - Base Facts
%% Defines basic desires for different personality types
desirable(cupid, wealth).         % Cupid desires wealth
desirable(ascetic, abstinence).   % Ascetic desires abstinence
desirable(coward, safety).        % Coward desires safety
desirable(brave, fight).          % Brave desires combat
desirable(personality, explore).   % All personalities desire exploration
desirable(personality, unconstrained). % All personalities desire freedom

%% Desire Inheritance
%% Allows desires to be inherited through personality hierarchy
desirable(Perso, Obj):-
    desirable(SupPerso, SupObj),
    generalize_pair(Perso, Obj, SupPerso, SupObj).

%% Motivation System - Base Facts
%% Defines what elements motivate different objectives
motivation(valuableitem, wealth).     % Valuable items motivate wealth-seeking
motivation(item, abstinence).         % Items relate to abstinence
motivation(monster, fight).           % Monsters motivate fighting
motivation(danger, safety).           % Dangers motivate safety-seeking
motivation(safecell, explore).        % Safe cells motivate exploration
motivation(visitedcell, explore).     % Visited cells motivate exploration
motivation(obstacle, unconstrained).  % Obstacles relate to freedom of movement

%% Motivation Inheritance
%% Allows motivations to be inherited through element hierarchy
motivation(Elem, Obj):-
    motivation(SupElem, SupObj),
    generalize_pair(Elem, Obj, SupElem, SupObj).

%% Objective Generation - Step 1
%% Generates objectives based on personality traits and motivations
genObjective(Elem1, Perso, Obj, Elem2):-
    has_personality_trait(Elem1, Perso),  % Entity has personality trait
    desirable(Perso, Obj),                % Personality desires objective
    motivation(Elem2, Obj),               % Element motivates objective
    distance(Elem1, Elem2).               % Elements are within range

%% Satisfaction System - Base Facts
%% Defines which actions satisfy which objectives
satisfy(wealth, pickup).           % Pickup satisfies wealth
satisfy(abstinence, discard).      % Discard satisfies abstinence
satisfy(safety, moveback).         % Moving back satisfies safety
satisfy(safety, attack).           % Attack can satisfy safety
satisfy(fight, attack).            % Attack satisfies fighting
satisfy(explore, move).            % Movement satisfies exploration
satisfy(unconstrained, bumpwall).  % Wall bumping relates to constraints

%% Satisfaction Inheritance
%% Allows satisfaction relationships to be inherited
satisfy(Obj, Act):-
    satisfy(SupObj, SupAct),
    generalize_pair(Obj, Act, SupObj, SupAct).

%% Inclination System - Base Facts
%% Defines how strongly personalities are inclined toward actions
encline(coward, moveback, 10).     % Coward very strongly inclined to retreat
encline(brave, attack, 9).         % Brave strongly inclined to attack
encline(cupid, interact, 5).       % Cupid strongly inclined to interact
encline(ascetic, interact, 3).     % Ascetic moderately inclined to interact
encline(personality, bumpwall, 2). % Basic inclination to bump walls
encline(personality, move, 1).     % Basic inclination to move

%% Inclination Inheritance
%% Allows inclinations to be inherited through personality hierarchy
encline(Perso, Act, Util):-
    encline(SupPerso, SupAct, Util),
    generalize_pair(Perso, Act, SupPerso, SupAct).

%% Action Generation - Step 2
%% Final step that generates actions based on personality, objectives, and inclinations
genAction(Elem1, Perso, Obj, Elem2, Act, Util):-
    encline(Perso, Act, Util),     % Personality is inclined toward action
    satisfy(Obj, Act),             % Action satisfies objective
    genObjective(Elem1, Perso, Obj, Elem2). % Objective is valid for situation