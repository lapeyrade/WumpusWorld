%% Element Ontology - Defines the hierarchical structure of game elements and their relationships
%% This file implements a Description Logic (DL) based ontology using Prolog rules

%% Dynamic and incremental tabling declarations for all main predicates
%% Ensures efficient querying and updates of the knowledge base
:- table (element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1,
    valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1,
    bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, pit/1, danger/1, cell/1,
    startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1) as (incremental, dynamic).

:- dynamic([element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1,
    valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1,
    bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, pit/1, danger/1, cell/1,
    startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1], [incremental(true)]).

:- discontiguous [data_concept/2, trap/1,hasaspect/2, dangerous/1, monster/1, hasphysicaltrait/2,
    bottomless/1, element/1, object/1, being/1, animal/1, trap/1, item/1, obstacle/1, wall/1,
    valuableitem/1, commonitem/1, unvaluableitem/1, plant/1, gold/1, rock/1, weapon/1,
    bow/1, sword/1, monster/1, human/1, dragon/1, wumpus/1, pit/1, danger/1, cell/1,
    startcell/1, visitedcell/1, safecell/1, dangerouscell/1, unknowncell/1].

%% Element Hierarchy
%% Main categories of elements in the game world:
%% - cell: different types of game board cells
%% - object: physical items and obstacles
%% - being: living entities in the world
%% - danger: hazardous elements
element(X) :- cell(X).
element(X) :- object(X).
element(X) :- being(X).
element(X) :- danger(X).

%% Object Hierarchy
%% Physical items in the world:
%% - trap: hazardous game elements
%% - item: collectible objects
%% - obstacle: blocking elements
object(X) :- trap(X).
object(X) :- item(X).
object(X) :- obstacle(X).

%% Being Hierarchy
%% Living entities in the game:
%% - animal: mobile creatures
%% - plant: static living elements
being(X) :- animal(X).
being(X) :- plant(X).

%% Item Categories
%% Classification of items by value and function:
%% - valuableitem: high-worth objects
%% - commonitem: standard objects
%% - unvaluableitem: low-worth objects
%% - weapon: combat tools
item(X) :- valuableitem(X).
item(X) :- commonitem(X).
item(X) :- unvaluableitem(X).
item(X) :- weapon(X).
obstacle(X) :- wall(X).

%% Living Beings Classification
%% Types of animals in the game:
%% - monster: hostile creatures
%% - human: player characters
animal(X) :- monster(X).
animal(X) :- human(X).

%% Item Subcategories
%% Specific types of items:
%% - gold: valuable treasure
%% - rock: basic obstacle
%% - bow/sword: weapon types
valuableitem(X) :- gold(X).
unvaluableitem(X) :- rock(X).
weapon(X) :- bow(X).
weapon(X) :- sword(X).

%% Description Logic Implementation
%% Rules implementing DL axioms through Skolemization

%% Danger Concept
%% Implements: danger ≡ ∃hasAspect.dangerous
%% Defines what makes an element dangerous
hasaspect(X,f1(X)) :- danger(X).
dangerous(f1(X)) :- danger(X).
danger(X) :- hasaspect(X,Y), dangerous(Y).

%% Aspect System
%% Defines relationships between aspects:
%% - character aspects
%% - physical traits
hasaspect(X,Y) :- hascharacter(X,Y).
hasaspect(X,Y) :- hasphysicaltrait(X,Y).

%% Monster Properties
%% Implements: monster ⊑ ∃hasCharacter.aggressive
%% Defines monster behavior and traits
hascharacter(X,f2(X)) :- monster(X).
aggressive(f2(X)) :- monster(X).
dangerous(X) :- aggressive(X).

%% Monster Definition
%% Implements: monster ← hasAspect(X,Y),monstrous(Y)
%% Classifies entities as monsters
monster(X) :- hasaspect(X,Y), monstrous(Y).
hasaspect(X,f3(X)) :- monster(X).
monstrous(f3(X)) :- monster(X).

%% Monstrous Traits
%% Defines characteristics that make an entity monstrous:
%% - oneeyed: cyclops-like trait
%% - throwflame: fire-breathing ability
monstrous(X) :- oneeyed(X).
monstrous(X) :- throwflame(X).

%% Specific Monster Types
%% Defines unique traits for different monsters:
%% - wumpus: one-eyed creature
%% - dragon: flame-throwing beast
hasphysicaltrait(X,f4(X)) :- wumpus(X).
hasphysicaltrait(X,f4(X)) :- dragon(X).
oneeyed(f4(X)) :- wumpus(X).
throwflame(f4(X)) :- dragon(X).

%% Hazard Properties
%% Defines characteristics of dangerous elements:
%% - pit: bottomless hazard
%% - trap: dangerous obstacle
hasphysicaltrait(X,f5(X)) :- pit(X).
bottomless(f5(X)) :- pit(X).

dangerous(X) :- bottomless(X).
hasaspect(X,f6(X)) :- trap(X).
bottomless(f6(X)) :- trap(X).
trap(X) :- hasaspect(X,Y), bottomless(Y).

%% Cell Classification
%% Different types of game board cells:
%% - startcell: initial player position
%% - safecell: confirmed safe location
%% - visitedcell: previously explored
%% - dangerouscell: known hazard
%% - unknowncell: unexplored area
cell(X) :- startcell(X).
cell(X) :- safecell(X).
cell(X) :- visitedcell(X).
cell(X) :- dangerouscell(X).
cell(X) :- unknowncell(X).

%% Example Individuals generated by Unity as assertz
%% Format: Type([ID, [X, Y]]) where X,Y are coordinates
% startcell([startcell, [2, 3]]).
% safecell([safecell, [3, 3]]).
% safecell([safecell, [1, 3]]).
% safecell([safecell, [2, 4]]).
% safecell([safecell, [2, 2]]).
% unknowncell([unknowncell, [4, 3]]).
% unknowncell([unknowncell, [0, 3]]).
% unknowncell([unknowncell, [2, 5]]).
% unknowncell([unknowncell, [2, 1]]).
% unknowncell([unknowncell, [3, 4]]).
% unknowncell([unknowncell, [3, 2]]).
% unknowncell([unknowncell, [1, 4]]).
% unknowncell([unknowncell, [1, 2]]).
% human([human0, [2, 3]]).
% wumpus([wumpus, [2, 3]]).