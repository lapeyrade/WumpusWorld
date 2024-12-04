:-consult([onto, element, action, personality, objective, localization]).
:- classification(element,1).
:- classification(objective,1).
:- classification(personality,1).
:- classification(action,1).

:- table (has_personality_trait/2, desirable/2, motivation/2, satisfy/2, encline/3, genObjective/4, genAction/6) as incremental.

:- dynamic([has_personality_trait/2, data_concept/2], [incremental(true)]).



% /* Decision making predicates and rules */


/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
has_personality_trait/2
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
% Coming from Unity C#

/* Definition rules */
% none

/* Arguments specialization/generalization rules 
has_personality_trait(Entity,Perso)
Specializable arguments:
- Entity : list of data
- Perso : concept from the personality ontology
Generalizable arguments:
- none
*/

has_personality_trait(Entity,SupPerso):-
    has_personality_trait(SupEntity,Perso),
    (Entity=SupEntity;data_concept(Entity,SupEntity);strictSubsumedBy(Entity,SupEntity)),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Entity\=SupEntity;Perso\=SupPerso).



/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
desirable/2
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
desirable(cupid, wealth).
desirable(ascetic, abstinence).
desirable(coward, safety).
desirable(brave, fight).
desirable(personality, explore).
desirable(personality, unconstrained).

/* Definition rules */
% none

/* Arguments specialization/generalization rules 
desirable(Perso,Obj)
Specializable arguments:
- Perso : concept from the personality ontology
- Obj : concept from the objective ontology
Generalizable arguments:
- none
*/

desirable(Perso,Obj):-
	desirable(SupPerso,SupObj),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
	(Perso\=SupPerso;Obj\=SupObj).

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
motivation/2
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
motivation(valuableitem, wealth).
motivation(item, abstinence).
motivation(monster, fight).
motivation(danger, safety).
motivation(safecell, explore).
motivation(visitedcell, explore).
motivation(obstacle, unconstrained).

/* Definition rules */
% none

/* Arguments specialization/generalization rules 
motivation(Elem,Obj)
Specializable arguments:
- Elem : concept from the element ontology
- Obj : concept from the objective ontology
Generalizable arguments:
- none
*/
motivation(Elem,Obj):-
	motivation(SupElem,SupObj),
    (Elem=SupElem;data_concept(Elem,SupElem);strictSubsumedBy(Elem,SupElem)),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
	(Elem\=SupElem;Obj\=SupObj).

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
genObjective/4 - Decision making step 1
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
% none

/* Definition rules */
genObjective(Elem1, Perso, Obj, Elem2):-
    has_personality_trait(Elem1, Perso), 
    desirable(Perso, Obj),
    motivation(Elem2, Obj),
    distance(Elem1, Elem2).

/* Arguments specialization/generalization rules 
genObjective(Elem1, Perso, Obj, Elem2)
Specializable arguments:
- none
Generalizable arguments:
- none
*/

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
satisfy/2
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
satisfy(wealth, pickup).
satisfy(abstinence, discard).
satisfy(safety, moveback).
satisfy(safety, attack).
satisfy(fight, attack).
satisfy(explore, move).
satisfy(unconstrained, bumpwall).

/* Definition rules */
% none

/* Arguments specialization/generalization rules 
satisfy(Obj, Act)
Specializable arguments:
- Obj : concept from the objective ontology
- Act : concept form the action ontology
Generalizable arguments:
- none
*/
satisfy(Obj, Act):-
	satisfy(SupObj,SupAct),
    (Obj=SupObj;data_concept(Obj,SupObj);strictSubsumedBy(Obj,SupObj)),
    (Act=SupAct;data_concept(Act,SupAct);strictSubsumedBy(Act,SupAct)),
	(Obj\=SupObj;Act\=SupAct).

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
encline/3
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
encline(cupid, interact, 5).
encline(ascetic, interact, 3).
encline(coward, moveback, 10).
encline(brave, attack, 9).
encline(personality, move, 1).
encline(personality, bumpwall, 2).

/* Definition rules */
% none

/* Arguments specialization/generalization rules 
encline(Perso, Act, Util)
Specializable arguments:
- Perso : concept from the personality ontology
- Act : concept form the action ontology
Generalizable arguments:
- none
*/
encline(Perso, Act, Util):-
	encline(SupPerso, SupAct, Util),
    (Perso=SupPerso;data_concept(Perso,SupPerso);strictSubsumedBy(Perso,SupPerso)),
    (Act=SupAct;data_concept(Act,SupAct);strictSubsumedBy(Act,SupAct)),
	(Perso\=SupPerso;Act\=SupAct).


/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
genAction/6 - Decision making step 2
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/* Facts */
% none

/* Definition rules */
genAction(Elem1, Perso, Obj, Elem2, Act, Util):-
    encline(Perso, Act, Util), 
    satisfy(Obj, Act),
    genObjective(Elem1, Perso, Obj, Elem2).

/* Arguments specialization/generalization rules 
genAction(Elem1, Perso, Obj, Elem2, Act, Util)
Specializable arguments:
- none
Generalizable arguments:
- none
*/




% % /*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
% % /*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
% % /*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
% %
% % Faits issus du jeu via C# 
% %
% % /*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
% % /*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
% % $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

% trait(human0, cupid).
% :-assertz(data_concept([human0, [1, 1]],human)).
% :-assertz(human([human0, [1, 1]])).
% :-assertz(has_personality_trait([human0, [1, 1]], cupid)).

% % trait(human0, coward).
% :-assertz(has_personality_trait([human0, [1, 1]], brave)).

% :-assertz(data_concept([gold1, [1, 1]],gold)).
% :-assertz(gold([gold1, [1, 1]])).
% :-assertz(data_concept([wumpus0, [1, 2]],wumpus)).
% :-assertz(wumpus([wumpus0, [1, 2]])).

% /* Test

% 2 ?- genAction(Elem1, Perso, Obj, Elem2, Act, Util).
% Elem1 = [human0, [1, 1]],
% Perso = cupid,
% Obj = wealth,
% Elem2 = [gold1, [1, 1]],
% Act = pickup,
% Util = 5 ;
% Elem1 = [human0, [1, 1]],
% Perso = brave,
% Obj = fight,
% Elem2 = [wumpus0, [1, 2]],
% Act = shoot,
% Util = 9 ;
% Elem1 = [human0, [1, 1]],
% Perso = brave,
% Obj = fight,
% Elem2 = [wumpus0, [1, 2]],
% Act = attack,
% Util = 9 ;
% Elem1 = [human0, [1, 1]],
% Perso = brave,
% Obj = fight,
% Elem2 = [wumpus0, [1, 2]],
% Act = shootarrow,
% Util = 9.
% */

