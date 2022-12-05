% :- module(element, [element/2]).

:- multifile [is_true/1, situation/2].

:- table element/2, valuable/1, common/1, unvaluable/1, valuable_object/1, common_object/1, unvaluable_object/1, object/1  as incremental.

%%%%% ONTOLOGY ELEMENT %%%%%
element(Id):- being(Id).
element(Id):- object(Id).
element(Id):- value(Id).

object(Id):- obstacle(Id).
object(Id):- trap(Id).
object(Id):- item(Id).

item(Id):- gold(Id).
item(Id):- iron(Id).
item(Id):- rock(Id).

object(Id):- valuable_object(Id).
object(Id):- common_object(Id).
object(Id):- unvaluable_object(Id).

value(Id):- valuable(Id).
value(Id):- common(Id).
value(Id):- unvaluable(Id).

valuable(Id):- gold(Id).
valuable(Id):- valuable_object(Id).
common(Id):- iron(Id).
common(Id):- common_object(Id).
unvaluable(Id):- rock(Id).
unvaluable(Id):- unvaluable_object(Id).

valuable_object(Id):- object(Id), valuable(Id).
common_object(Id):- object(Id), common(Id).
unvaluable_object(Id):- object(Id), unvaluable(Id).

being(Id):- animal(Id).

animal(Id):- monster(Id).
animal(Id):- human(Id).
obstacle(Id):- wall(Id).
monster(Id):- wumpus(Id).
monster(Id):- dragon(Id).
trap(Id):- pit(Id).

% True for all Id starting with "human"
human(Id):- situation(Id, _),
    sub_string(Id, 0, 5, _, human).

% True for all Id starting with "dog"
dog(Id):- situation(Id, _),
    sub_string(Id, 0, 3, _, dog).

% True for all Id starting with "wumpus"
% except for "wumpusno", "wumpusyes", "wumpusdead"
wumpus(Id):- is_true(situation(Id, _)),
    sub_string(Id, 0, 6, _, wumpus),
    Id \= wumpusno, Id \= wumpusyes,
    Id \= wumpusdead.

% True for all Id starting with "dragon"
dragon(Id):- situation(Id, _),
    sub_string(Id, 0, 6, _, dragon).

% True for all Id starting with "pit"
% except for "pitpo", "pityes"
pit(Id):- is_true(situation(Id, _)),
    sub_string(Id, 0, 3, 0, pit),
    Id \= pitno, Id \= pityes.

% True for all Id starting with "wall"
wall(Id):- situation(Id, _),
    sub_string(Id, 0, 4, _, wall).

% True for all Id starting with "gold"
gold(Id):- situation(Id, _),
    sub_string(Id, 0, 4, _, gold).

% True for all Id starting with "iron"
iron(Id):- situation(Id, _),
    sub_string(Id, 0, 4, _, iron).

% True for all Id starting with "rock"
rock(Id):- situation(Id, _),
    sub_string(Id, 0, 4, _, rock).