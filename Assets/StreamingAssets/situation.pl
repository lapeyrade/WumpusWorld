:- multifile [query_ontology/3].
:- discontiguous [element/1, being/1, object/1, valuable/1, common/1, unvaluable/1].
:- table situation/2, element/1, valuable/1, common/1, unvaluable/1, valuable_object/1, common_object/1, unvaluable_object/1, object/1 as incremental.
:- dynamic([situation/2]).

%-----------------------------------
/* Ontology Element */
    /* Hierarchy of concepts */
    element(X):- being(X).
    element(X):- object(X).
    object(X):- obstacle(X).
    object(X):- trap(X).
    object(X):- weapon(X).
    object(X):- item(X).
    being(X):- animal(X).
    item(X):- gold(X).
    item(X):- iron(X).
    item(X):- rock(X).
    item(X):- weapon(X).
    weapon(X):- bow(X).
    weapon(X):- arrow(X).
    weapon(X):- sword(X).
    animal(X):- monster(X).
    animal(X):- agent(X).
    obstacle(X):- wall(X).
    monster(X):- wumpus(X).
    monster(X):- dragon(X).
    trap(X):- pit(X).

    % object IS (Value) %
    object(X):- value(X).
    value(X):- valuable(X).
    value(X):- common(X).
    value(X):- unvaluable(X).
    valuable(X):- valuable_object(X).
    common(X):- common_object(X).
    unvaluable(X):- unvaluable_object(X).
    valuable_object(X):- object(X), valuable(X).
    common_object(X):- object(X), common(X).
    unvaluable_object(X):- object(X), unvaluable(X).
    valuable(X):- gold(X).
    common(X):- iron(X).
    unvaluable(X):- rock(X).

    % element IS (danger) %
    element(X):- danger(X).
    danger(X):- monster(X).
    danger(X):- trap(X).
    danger(X):- weapon(X).

/* Entry point */
    situation(Element, X):- query_ontology(Element, element, X).

/* Exit point - Notion constant-predicate */
    agent([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 5, _, agent).
    dog([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 3, _, dog).
    wumpus([Id, [X,Y]]):- situation(Id, [X,Y]),
        sub_string(Id, 0, 6, _, wumpus), Id \= wumpusdead.
    dragon([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 6, _, dragon).
    pit([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 3, 0, pit).
    wall([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 4, _, wall).
    gold([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 4, _, gold).
    iron([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 4, _, iron).
    rock([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 4, _, rock).
    bow([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 3, _, bow).
    sword([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 5, _, sword).
    arrow([Id, [X,Y]]):-
        situation(Id, [X,Y]), sub_string(Id, 0, 5, _, arrow).
%-----------------------------------