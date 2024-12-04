:- table (element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1, valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1, bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, bat/1, pit/1, danger/1, cell/1, startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1) as (incremental, dynamic).

/*** Elements Hierachy ***/
element(X) :- cell(X).
element(X) :- object(X).
element(X) :- being(X).
element(X) :- danger(X).
object(X) :- trap(X).
object(X) :- item(X).
object(X) :- obstacle(X).
being(X) :- animal(X).
being(X) :- plant(X).
danger(X) :- monster(X).
danger(X) :- trap(X).
trap(X) :- pit(X).
item(X) :- valuableitem(X).
item(X) :- commonitem(X).
item(X) :- unvaluableitem(X).
item(X) :- weapon(X).
obstacle(X) :- wall(X).
animal(X) :- monster(X).
animal(X) :- human(X).
valuableitem(X) :- gold(X).
unvaluableitem(X) :- rock(X).
weapon(X) :- bow(X).
weapon(X) :- sword(X).
monster(X) :- dragon(X).
monster(X) :- wumpus(X).
monster(X) :- bat(X).

/*** Cell Hierachy ***/
cell(X) :- startcell(X).
cell(X) :- safecell(X).
cell(X) :- visitedcell(X).
cell(X) :- dangerouscell(X).
cell(X) :- unknowncell(X).

/* Some individuals */
% human([human0, [1, 1]]).
% human([human1, [1, 1]]).
% human([human2, [1, 1]]).
% human([human3, [1, 1]]).
% dragon([dragon0, [1, 1]]).
% wumpus([wumpus, [1, 1]]).
% gold([gold, [1, 1]]).
% rock([rock0, [1, 1]]).