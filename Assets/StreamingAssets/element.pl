:- table (element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1, valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1, bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, pit/1, danger/1, cell/1, startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1) as (incremental, dynamic).

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

% Skolemization examples translated from French
% Translation of "danger ≡ ∃hasAspect.dangerous"
% i.e., "danger(X) ↔ hasAspect(X,Y),dangerous(Y)"
hasaspect(X,f1(X)) :- danger(X).
dangerous(f1(X)) :- danger(X).
danger(X) :- hasaspect(X,Y), dangerous(Y).

% Role hierarchy for the new predicates
hasaspect(X,Y) :- hascharacter(X,Y).
hasaspect(X,Y) :- hasphysicaltrait(X,Y).


% Translation of "monster ⊑ ∃hasCharacter.aggressive"
% i.e., "hasCharacter(X,Y), aggressive(Y) ← monster(X)"
hascharacter(X,f2(X)) :- monster(X).
aggressive(f2(X)) :- monster(X).
dangerous(X) :- aggressive(X).

% Translation of "monster ← hasAspect(X,Y),monstrous(Y)"
monster(X) :- hasaspect(X,Y), monstrous(Y).
hasaspect(X,f3(X)) :- monster(X).
monstrous(f3(X)) :- monster(X).

monstrous(X) :- oneeyed(X).
monstrous(X) :- throwflame(X).

% Translation of "hasPhysicalTrait(X,Y), oneEyed(Y) ← wumpus(X)"
hasphysicaltrait(X,f4(X)) :- wumpus(X).
hasphysicaltrait(X,f4(X)) :- dragon(X).
oneeyed(f4(X)) :- wumpus(X).
throwflame(f4(X)) :- dragon(X).

hasphysicaltrait(X,f5(X)) :- pit(X).
bottomless(f5(X)) :- pit(X).

dangerous(X) :- bottomless(X).
hasaspect(X,f6(X)) :- trap(X).
bottomless(f6(X)) :- trap(X).
trap(X) :- hasaspect(X,Y), bottomless(Y).

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