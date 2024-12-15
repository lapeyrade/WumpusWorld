:-consult([onto, element, action, personality, objective, localization]).
:- classification(element,1), classification(objective,1), classification(personality,1), classification(action,1).

:- table (has_personality_trait/2, desirable/2, motivation/2,
    satisfy/2, encline/3, genObjective/4, genAction/6) as incremental.

:- dynamic([has_personality_trait/2, data_concept/2], [incremental(true)]).


/* Decision making predicates and rules */

% Facts
% Coming from Unity C#

% Generalization rule
has_personality_trait(Entity,SupPerso):-
    has_personality_trait(SupEntity,Perso),
    (Entity=SupEntity;data_concept(Entity,SupEntity);strictSubsumedBy(Entity,SupEntity)),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Entity\=SupEntity;Perso\=SupPerso).

% Facts
desirable(cupid, wealth).
desirable(ascetic, abstinence).
desirable(coward, safety).
desirable(brave, fight).
desirable(personality, explore).
desirable(personality, unconstrained).

% Generalization rule
desirable(Perso,Obj):-
	desirable(SupPerso,SupObj),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
	(Perso\=SupPerso;Obj\=SupObj).

% Facts
motivation(valuableitem, wealth).
motivation(item, abstinence).
motivation(monster, fight).
motivation(danger, safety).
motivation(safecell, explore).
motivation(visitedcell, explore).
motivation(obstacle, unconstrained).

% Generalization rule
motivation(Elem,Obj):-
	motivation(SupElem,SupObj),
    (Elem=SupElem;data_concept(Elem,SupElem);strictSubsumedBy(Elem,SupElem)),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
	(Elem\=SupElem;Obj\=SupObj).

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

% Generalization rule
satisfy(Obj, Act):-
	satisfy(SupObj,SupAct),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
    (Act=SupAct;data_concept(Act,SupAct);strictSubsumedBy(Act,SupAct)),
	(Obj\=SupObj;Act\=SupAct).

% Facts
encline(cupid, interact, 5).
encline(ascetic, interact, 3).
encline(coward, moveback, 10).
encline(brave, attack, 9).
encline(personality, move, 1).
encline(personality, bumpwall, 2).

% Generalization rule
encline(Perso, Act, Util):-
	encline(SupPerso, SupAct, Util),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Act=SupAct;data_concept(Act,SupAct);strictSubsumedBy(Act,SupAct)),
	(Perso\=SupPerso;Act\=SupAct).

% Definition rule - Step 2
genAction(Elem1, Perso, Obj, Elem2, Act, Util):-
    encline(Perso, Act, Util), 
    satisfy(Obj, Act),
    genObjective(Elem1, Perso, Obj, Elem2).