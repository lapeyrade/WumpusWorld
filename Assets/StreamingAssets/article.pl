:- table (subClassOf/2, trait/2, desirable/2, location/2, motivation/2,
    satisfy/2, encline/2, genObjective/4, genAction/5) as incremental.

:- dynamic([subClassOf/2, location/2, trait/2], [incremental(true)]).

subClassOf(X, Y):-
    subClassOf(X, Z), subClassOf(Z, Y).

/*** Elements Hierachy ***/
subClassOf(object, element).
subClassOf(animal, being).
subClassOf(trap, object).
subClassOf(item, object).
subClassOf(obstacle, object).
subClassOf(wall, obstacle).
subClassOf(valuable_item, item).
subClassOf(common_item, item).
subClassOf(unvaluable_item, item).
subClassOf(being, element).
subClassOf(plant, being).
subClassOf(gold, valuable_item).
subClassOf(rock, unvaluable_item).
subClassOf(weapon, item).
subClassOf(bow, weapon).
subClassOf(sword, weapon).
subClassOf(monster, animal).
subClassOf(human, animal).
subClassOf(dragon, monster).
subClassOf(wumpus, monster).
subClassOf(pit, trap).
subClassOf(monster, danger).
subClassOf(trap, danger).

subClassOf(visited, safe).

/* Some individuals */
subClassOf(human0, human).
subClassOf(human1, human).
subClassOf(human2, human).
subClassOf(human3, human).
subClassOf(dragon0, dragon).
subClassOf(wumpus, wumpus).
subClassOf(gold, gold).
subClassOf(rock0, rock).

/*** Actions Hierachy ***/
subClassOf(interact, action).
subClassOf(fight, action).
subClassOf(move, action).
subClassOf(move_back, action).
subClassOf(bump_wall, action).
subClassOf(pick, interact).
subClassOf(drop, interact).
subClassOf(shoot, fight).
subClassOf(shoot_arrow, shoot).

/*** Personalities Hierachy ***/
subClassOf(ambitious, personality).
subClassOf(disciplined, personality).
subClassOf(sensitive, personality).
subClassOf(cupid, ambitious).
subClassOf(brave, ambitious).
subClassOf(ascetic, disciplined).
subClassOf(coward, sensitive).

/*** Objectives Hierachy ***/
subClassOf(success, objective).
subClassOf(healthiness, objective).
subClassOf(unconstrained, objective).
subClassOf(explore, objective).
subClassOf(wealth, success).
subClassOf(fight, success).
subClassOf(abstinence, healthiness).
subClassOf(safety, healthiness).

/* Link Element-Personality */
% trait(human0, cupid).
% trait(human0, ascetic).
% trait(human0, brave).
% trait(human0, coward).

% trait(human1, ascetic).
% trait(human2, brave).
% trait(human3, coward).
% trait(monster, brave).
% trait(dragon, cupid).
trait(SubE, P):- 
    trait(E, P), subClassOf(SubE, E).
trait(E, SubP):- 
    trait(E, P), subClassOf(SubP, P).

/* Link Personality-Objective */
desirable(cupid, wealth).
desirable(ascetic, abstinence).
desirable(coward, safety).
desirable(brave, fight).
desirable(personality, explore).
desirable(personality, unconstrained).
desirable(SubP, O):- 
    desirable(P, O), subClassOf(SubP, P).
desirable(P, SubO):- 
    desirable(P, O), subClassOf(SubO, O).

/* Link Element-Objective */
motivation(valuable_item, wealth).
motivation(common_item, abstinence).
motivation(unvaluable_item, abstinence).
motivation(monster, fight).
motivation(danger, safety).
motivation(safe, explore).
motivation(obstacle, unconstrained).
motivation(SubE, O):-
    motivation(E, O), subClassOf(SubE, E).
motivation(E, SubO):-
    motivation(E, O), subClassOf(SubO, O).

/* Link Element-Location */
% location(human0, [1, 1]).
% location(human1, [1, 1]).
% location(human2, [1, 1]).
% location(human3, [1, 1]).
% location(gold0, [1, 1]).
% location(gold1, [2, 2]).
% location(rock0, [1, 1]).
% location(dragon0, [1, 1]).
% location(safe, [1, 1]).
location(SubE, L):-
  location(E, L), subClassOf(SubE, E).

/*** Generate Objectives ***/
genObjective(Elem1, P, Obj, Elem2):-
    trait(Elem1, P), desirable(P, Obj),
    motivation(Elem2, Obj),
    same_or_adjacent_cell(Elem1, Elem2).

same_or_adjacent_cell(Elem1, Elem2):-
    location(Elem1, [X1, Y1]),
    location(Elem2, [X2, Y2]),
    Elem1 \= Elem2,
    (
        X1 is X2, Y1 is Y2;
        X1 is X2, Y1 is Y2 + 1;
        X1 is X2, Y1 is Y2 - 1;
        X1 is X2 + 1, Y1 is Y2;
        X1 is X2 - 1, Y1 is Y2
    ).

/* Link Objective-Action */
satisfy(wealth, pick).
satisfy(abstinence, drop).
satisfy(safety, move_back).
satisfy(safety, attack).
satisfy(fight, attack).
satisfy(explore, move).
satisfy(unconstrained, bump_wall).
satisfy(SubO, A):- 
    satisfy(O, A), subClassOf(SubO, O).
satisfy(O, SubA):-
    satisfy(O, A), subClassOf(SubA, A).

/* Link Personality-Action */
encline(cupid, interact).
encline(ascetic, interact).
encline(coward, move_back).
encline(brave, attack).
encline(personality, move).
encline(personality, bump_wall).
encline(SubP, A):-
    encline(P, A), subClassOf(SubP, P).
encline(P, SubA):-
    encline(P, A), subClassOf(SubA, A).

/*** Generate Actions ***/
genAction(Elem1, Perso, Obj, Elem2, Act):-
  encline(Perso, Act), satisfy(Obj, Act),
  genObjective(Elem1, Perso, Obj, Elem2).