% situation.pl
:- multifile [situation/2, trait/2, query_ontology/3].
:- discontiguous [action/1, possible_action/2, situation_action/2, objective_action_situation/3].
:- table objective_action_situation/3, possible_action/2, situation_action/2 as incremental.

%-----------------------------------
/* Ontology Action */
    /* Hierarchy of concepts */
    action(X):- move(X).
    action(X):- interact(X).
    action(X):- fight(X).
    move(X):- move_back(X).
    move(X):- move_up(X).
    move(X):- move_down(X).
    move(X):- move_right(X).
    move(X):- move_left(X).
    interact(X):- pick_up(X).
    interact(X):- discard(X).
    fight(X):- shoot(X).
    shoot(X):- shoot_arrow(X).
    shoot_arrow(X):- shoot_arrow_up(X).
    shoot_arrow(X):- shoot_arrow_down(X).
    shoot_arrow(X):- shoot_arrow_right(X).
    shoot_arrow(X):- shoot_arrow_left(X).

    /* Entry point */
    possible_action(Action, X):-
        query_ontology(X, action, Action).
    situation_action(Situation, Action):-
        query_ontology(Situation, action, Action).
    objective_action_situation(Obj, Act, Sit):-
        query_ontology([Obj, Sit], action, Act).

    /* Exit point - Notion constant-predicate */
    move_back([Sit,X]):- possible_action(move_back, [Sit,X]).
    move_back(Sit):- situation_action(Sit, move_back).
    move_back([Obj, Sit]):- objective_action_situation(Obj, move_back, Sit).
    move_right([Sit,X]):- possible_action(move_right, [Sit,X]).
    move_right(Sit):- situation_action(Sit, move_right).
    move_right([Obj, Sit]):- objective_action_situation(Obj, move_right, Sit).
    move_left([Sit,X]):- possible_action(move_left, [Sit,X]). 
    move_left(Sit):- situation_action(Sit, move_left).
    move_left([Obj, Sit]):- objective_action_situation(Obj, move_left, Sit).
    move_up([Sit,X]):- possible_action(move_up, [Sit,X]).
    move_up(Sit):- situation_action(Sit, move_up).
    move_up([Obj, Sit]):- objective_action_situation(Obj, move_up, Sit).
    move_down([Sit,X]):- possible_action(move_down, [Sit,X]).
    move_down(Sit):- situation_action(Sit, move_down).
    move_down([Obj, Sit]):- objective_action_situation(Obj, move_down, Sit).
    pick_up([Sit,X]):- possible_action(pick_up, [Sit,X]).
    pick_up(Sit):- situation_action(Sit, pick_up).
    pick_up([Obj, Sit]):- objective_action_situation(Obj, pick_up, Sit).
    discard([Sit, X]):-possible_action(discard, [Sit,X]).
    discard(Sit):- situation_action(Sit, discard).
    discard([Obj, Sit]):- objective_action_situation(Obj, discard, Sit).
    shoot_arrow_up([Sit,X]):- possible_action(shoot_arrow_up, [Sit,X]).
    shoot_arrow_up(Sit):- situation_action(Sit, shoot_arrow_up).
    shoot_arrow_up([Obj, Sit]):- objective_action_situation(Obj, shoot_arrow_up, Sit).
    shoot_arrow_down([Sit,X]):- possible_action(shoot_arrow_down, [Sit,X]).
    shoot_arrow_down(Sit):- situation_action(Sit, shoot_arrow_down).
    shoot_arrow_down([Obj, Sit]):- objective_action_situation(Obj, shoot_arrow_down, Sit).
    shoot_arrow_right([Sit,X]):- possible_action(shoot_arrow_right, [Sit,X]).
    shoot_arrow_right(Sit):- situation_action(Sit, shoot_arrow_right).
    shoot_arrow_right([Obj, Sit]):- objective_action_situation(Obj, shoot_arrow_right, Sit).
    shoot_arrow_left([Sit,X]):- possible_action(shoot_arrow_left, [Sit,X]).
    shoot_arrow_left(Sit):- situation_action(Sit, shoot_arrow_left).
    shoot_arrow_left([Obj, Sit]):- objective_action_situation(Obj, shoot_arrow_left, Sit).

%-----------------------------------

/* Generic Action Predicate */
possible_action(Action, [Situation, X]):-
    % situation(Agent, _),  
    situation(Situation, X),
    situation_action(Situation, Action),
    trait(_, Trait),
    trait_objective(Trait, Objective),
    objective_action_situation(Objective, Action, Situation).
    % writeln('').

situation_action(object, pick_up).
situation_action(object, discard).
situation_action(danger, move_back).
situation_action(danger, move_right).
situation_action(danger, move_left).
situation_action(danger, move_up).
situation_action(danger, move_down).
situation_action(monster, shoot_arrow_up).
situation_action(monster, shoot_arrow_down).
situation_action(monster, shoot_arrow_right).
situation_action(monster, shoot_arrow_left).

trait_objective(cupid, wealth).
% trait_objective(reckless, combat).
% trait_objective(fearful, safety).

objective_action_situation(wealth, pick_up, valuable_object).
objective_action_situation(wealth, discard, unvaluable_object).
% objective_action_situation(wealth, pick_up, object).
objective_action_situation(safety, move_back, danger).
objective_action_situation(safety, move_right, danger).
objective_action_situation(safety, move_left, danger).
objective_action_situation(safety, move_up, danger).
objective_action_situation(safety, move_down, danger).
objective_action_situation(combat, shoot_arrow_right, monster).
objective_action_situation(combat, shoot_arrow_left, monster).
objective_action_situation(combat, shoot_arrow_up, monster).
objective_action_situation(combat, shoot_arrow_down, monster).
% objective_action_situation(combat, fight, danger).