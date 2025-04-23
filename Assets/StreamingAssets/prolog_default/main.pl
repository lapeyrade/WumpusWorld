%% Main Decision System - Core reasoning and behavior rules
%% This file implements the central decision-making system for agent behavior

%% Module Dependencies
%% Required ontologies and knowledge bases:
%% - element: game world elements and relationships
%% - action: available actions and their effects
%% - personality: character traits and tendencies
%% - objective: goals and motivations
%% - localization: spatial relationships
:-consult([element, action, personality, objective, localization]).

%% Dynamic and incremental tabling declarations for reasoning predicates
%% Ensures efficient querying and updates of the behavioral knowledge base
:- table (desirable/2, motivation/2, satisfy/2, encline/3, genObjective/4, genAction/6,
        data_concept/2, subsumedBy/2) as (incremental, dynamic).

%% Subsumption System
%% Implements hierarchical relationships between concepts and transitive subsumption
%% - Transitive relationships through recursive rules
%% - Concept inheritance and instance classification 
%% - Handles direct matches and indirect relationships
subsumedBy(Specific, General) :-
    nonvar(Specific),
    nonvar(General),
    GeneralPred =.. [General, _],
    clause(GeneralPred, BodyTerm),
    BodyTerm =.. [Intermediate|_],
    (Specific = Intermediate;  % Direct match
     subsumedBy(Specific, Intermediate)).  % Recursive check

%% Data Concept System
%% Implements instance classification:
%% - Maps instances to their concepts
%% - Supports concept membership testing
data_concept(Instance, Concept) :-
    nonvar(Instance),
    nonvar(Concept),
    Predicate =.. [Concept, Instance],
    call(Predicate).

data_concept([wumpus, [2, 2]], monster),
    wumpus([wumpus, [2, 2]]),
    danger([wumpus, [2, 2]]).


%% Desire System
%% Base desires for different personality types:
%% - cupid: wealth-seeking behavior
%% - ascetic: abstinence from items
%% - coward: safety-prioritizing
%% - brave: combat-seeking
%% - general: exploration and freedom
desirable(cupid, wealth).
desirable(ascetic, abstinence).
desirable(coward, safety).
desirable(brave, fight).
desirable(personality, explore).
desirable(personality, unconstrained).

%% Desire Inheritance Rules
%% Implements desire propagation through hierarchies:
%% - Personality-based inheritance
%% - Objective-based inheritance
desirable(Perso, Obj):-
    desirable(GenPerso, Obj), 
    subsumedBy(Perso, GenPerso).

desirable(Perso, Obj):-
    desirable(Perso, GenObj), 
    subsumedBy(Obj, GenObj).

%% Motivation System
%% Element-objective relationships:
%% - valuableitem → wealth
%% - item → abstinence
%% - monster → fight
%% - danger → safety
%% - cells → exploration
%% - obstacle → freedom
motivation(valuableitem, wealth).
motivation(item, abstinence).
motivation(monster, fight).
motivation(danger, safety).
motivation(safecell, explore).
motivation(visitedcell, explore).
motivation(obstacle, unconstrained).

%% Motivation Inheritance Rules
%% Implements motivation propagation:
%% - Element-based inheritance
%% - Objective-based inheritance
motivation(Elem, Obj):-
    motivation(GenElem, Obj), 
    subsumedBy(Elem, GenElem).

motivation(Elem, Obj):-
    motivation(Elem, GenObj), 
    subsumedBy(Obj, GenObj).

%% Objective Generation System - Step 1
%% Generates valid objectives based on:
%% - Personality desires
%% - Element motivations
%% - Spatial relationships
genObjective([Elem1, [X1, Y1]], Perso, Obj, [Elem2, [X2, Y2]]):-
    desirable(Perso, Obj),
    motivation(Elem2, Obj),
    data_concept([Elem1, [X1, Y1]], Perso),
    distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

%% Satisfaction System
%% Action-objective relationships:
%% - pickup → wealth
%% - discard → abstinence
%% - moveback/attack → safety
%% - attack → fight
%% - move → explore
%% - bumpwall → freedom
satisfy(wealth, pickup).
satisfy(abstinence, discard).
satisfy(safety, moveback).
satisfy(safety, attack).
satisfy(fight, attack).
satisfy(explore, move).
satisfy(unconstrained, bumpwall).

%% Satisfaction Inheritance Rules
%% Implements satisfaction propagation:
%% - Objective-based inheritance
%% - Action-based inheritance
satisfy(Obj, Act):-
    satisfy(GenObj, Act), 
    subsumedBy(Obj, GenObj).

satisfy(Obj, Act):-
    satisfy(Obj, GenAct), 
    subsumedBy(GenAct, Act).

%% Utility System
%% Personality-action preferences:
%% - coward: strong retreat (10)
%% - brave: strong attack (9)
%% - cupid: strong interaction (5)
%% - ascetic: moderate interaction (3)
%% - general: basic movement (1-2)
encline(coward, moveback, 10).
encline(brave, attack, 9).
encline(cupid, interact, 5).
encline(ascetic, interact, 3).
encline(personality, bumpwall, 2).
encline(personality, move, 1).

%% Inclination Inheritance Rules
%% Implements preference propagation:
%% - Personality-based inheritance
%% - Action-based inheritance
encline(Perso, Act, Util):-
    encline(GenPerso, Act, Util), 
    subsumedBy(Perso, GenPerso).

encline(Perso, Act, Util):-
    encline(Perso, GenAct, Util), 
    subsumedBy(Act, GenAct).

%% Action Generation System - Step 2
%% Final decision-making process based on:
%% - Objective satisfaction
%% - Personality inclinations
%% - Valid objectives
genAction([Elem1, [X1, Y1]], Perso, Obj, Elem2, Act, Util):-
    satisfy(Obj, Act),
    encline(Perso, Act, Util),
    genObjective([Elem1, [X1, Y1]], Perso, Obj, Elem2).