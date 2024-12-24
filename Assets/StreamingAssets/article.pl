:-consult([onto, element, action, personality, objective, localization]).
:- classification(element,1), classification(objective,1), classification(personality,1), classification(action,1).

:- table (subsumes_or_equals/2, generalize_pair/4, has_personality_trait/2, desirable/2, motivation/2, satisfy/2, encline/3, genObjective/4, genAction/6) as (incremental, dynamic).

:- dynamic([has_personality_trait/2, data_concept/2], [incremental(true)]).

/* Decision making predicates and rules */
subsumes_or_equals(X, Y) :-
    X = Y;
    strictSubsumedBy(X, Y); 
    data_concept(X, Y). 

% Helper predicate for generalization pattern
generalize_pair(X1, X2, SupX1, SupX2) :-
    subsumes_or_equals(X1, SupX1),
    subsumes_or_equals(X2, SupX2),
    (X1 \= SupX1; X2 \= SupX2).

% Facts
% Coming from Unity C#

% Optimized generalization rules using helper
has_personality_trait(Entity, SupPerso):-
    has_personality_trait(SupEntity, Perso),
    generalize_pair(Entity, Perso, SupEntity, SupPerso).

% Facts
desirable(cupid, wealth).
desirable(ascetic, abstinence).
desirable(coward, safety).
desirable(brave, fight).
desirable(personality, explore).
desirable(personality, unconstrained).

desirable(Perso, Obj):-
    desirable(SupPerso, SupObj),
    generalize_pair(Perso, Obj, SupPerso, SupObj).

% Facts
motivation(valuableitem, wealth).
motivation(item, abstinence).
motivation(monster, fight).
motivation(danger, safety).
motivation(safecell, explore).
motivation(visitedcell, explore).
motivation(obstacle, unconstrained).

motivation(Elem, Obj):-
    motivation(SupElem, SupObj),
    generalize_pair(Elem, Obj, SupElem, SupObj).

% Definition rule - Step 1
genObjective(Elem1, Perso, Obj, Elem2):-
    has_personality_trait(Elem1, Perso), 
    desirable(Perso, Obj),
    motivation(Elem2, Obj),
    distance(Elem1, Elem2).

% Facts
satisfy(wealth, pickup).
satisfy(abstinence, discard).
satisfy(safety, moveback).
satisfy(safety, attack).
satisfy(fight, attack).
satisfy(explore, move).
satisfy(unconstrained, bumpwall).

satisfy(Obj, Act):-
    satisfy(SupObj, SupAct),
    generalize_pair(Obj, Act, SupObj, SupAct).

% Facts
encline(cupid, interact, 5).
encline(ascetic, interact, 3).
encline(coward, moveback, 10).
encline(brave, attack, 9).
encline(personality, move, 1).
encline(personality, bumpwall, 2).

encline(Perso, Act, Util):-
    encline(SupPerso, SupAct, Util),
    generalize_pair(Perso, Act, SupPerso, SupAct).

% Definition rule - Step 2
genAction(Elem1, Perso, Obj, Elem2, Act, Util):-
    encline(Perso, Act, Util), 
    satisfy(Obj, Act),
    genObjective(Elem1, Perso, Obj, Elem2).