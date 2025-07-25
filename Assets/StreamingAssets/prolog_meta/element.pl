%% Element Ontology - Defines the hierarchical structure of game elements and their relationships
%% This file implements a Description Logic (DL) based ontology using Prolog rules

%% Dynamic and incremental tabling declarations for all main predicates
%% This ensures efficient querying and updates of the knowledge base
:- table (element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1,
    valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1,
    bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, pit/1, danger/1, cell/1,
    startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1) as (incremental, dynamic).

%% Main Element Hierarchy
%% Defines the top-level categories and their relationships
%% element is the root concept, with four main branches: cell, object, being, and danger
element(X) :- cell(X).
element(X) :- object(X).
element(X) :- being(X).
element(X) :- danger(X).

%% Object Hierarchy
%% Objects are physical items in the world: traps, items, and obstacles
object(X) :- trap(X).
object(X) :- item(X).
object(X) :- obstacle(X).

%% Being Hierarchy
%% Beings are living entities: animals and plants
being(X) :- animal(X).
being(X) :- plant(X).

%% Detailed Item Categories
%% Items are classified by their value and function
trap(X) :- pit(X).
item(X) :- valuableitem(X).
item(X) :- commonitem(X).
item(X) :- unvaluableitem(X).
item(X) :- weapon(X).
obstacle(X) :- wall(X).

%% Living Beings Classification
animal(X) :- monster(X).
animal(X) :- human(X).

%% Item Subcategories
valuableitem(X) :- gold(X).
unvaluableitem(X) :- rock(X).
weapon(X) :- bow(X).
weapon(X) :- sword(X).

%% Description Logic Axioms Implementation
%% The following rules implement DL axioms using Skolemization

%% Danger Concept Definition
%% Implements: danger ≡ ∃hasAspect.dangerous
%% An element is dangerous if it has a dangerous aspect
hasaspect(X,f1(X)) :- danger(X).
dangerous(f1(X)) :- danger(X).
danger(X) :- hasaspect(X,Y), dangerous(Y).

%% Aspect Hierarchy
%% Defines relationships between different types of aspects
hasaspect(X,Y) :- hascharacter(X,Y).
hasaspect(X,Y) :- hasphysicaltrait(X,Y).

%% Monster Characteristics
%% Implements: monster ⊑ ∃hasCharacter.aggressive
%% All monsters have an aggressive character trait
hascharacter(X,f2(X)) :- monster(X).
aggressive(f2(X)) :- monster(X).
dangerous(X) :- aggressive(X).

%% Monster Definition
%% Implements: monster ← hasAspect(X,Y),monstrous(Y)
%% An entity is a monster if it has monstrous aspects
monster(X) :- hasaspect(X,Y), monstrous(Y).
hasaspect(X,f3(X)) :- monster(X).
monstrous(f3(X)) :- monster(X).

%% Monstrous Traits Definition
monstrous(X) :- oneeyed(X).
monstrous(X) :- throwflame(X).

%% Specific Monster Types Properties
%% Defines characteristics of Wumpus and Dragon
hasphysicaltrait(X,f4(X)) :- wumpus(X).
hasphysicaltrait(X,f4(X)) :- dragon(X).
oneeyed(f4(X)) :- wumpus(X).
throwflame(f4(X)) :- dragon(X).

%% Pit and Trap Properties
%% Defines characteristics of pits and traps
hasphysicaltrait(X,f5(X)) :- pit(X).
bottomless(f5(X)) :- pit(X).

dangerous(X) :- bottomless(X).
hasaspect(X,f6(X)) :- trap(X).
bottomless(f6(X)) :- trap(X).
trap(X) :- hasaspect(X,Y), bottomless(Y).

%% Cell Classification System
%% Defines different types of cells in the game world
cell(X) :- startcell(X).
cell(X) :- safecell(X).
cell(X) :- visitedcell(X).
cell(X) :- dangerouscell(X).
cell(X) :- unknowncell(X).

%% Example Individuals
%% Format: Type([ID, [X, Y]]) where X,Y are coordinates
% human([human0, [1, 1]]).
% human([human1, [1, 1]]).
% human([human2, [1, 1]]).
% human([human3, [1, 1]]).
% dragon([dragon0, [1, 1]]).
% wumpus([wumpus, [1, 1]]).
% gold([gold, [1, 1]]).
% rock([rock0, [1, 1]]).