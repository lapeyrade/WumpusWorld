%% Localization System - Defines spatial relationships and distance calculations
%% This file implements the rules for determining spatial relationships between game elements

%% Allow predicates to be defined in any order
:- discontiguous data_concept/2.

%% Dynamic and incremental tabling declarations for localization predicates
%% Ensures efficient querying and updates of the spatial knowledge base
:- table (distance/2, same_cell/2, adjacent_cell/2) as (incremental, dynamic).

%% Localization Rules
%% Rules for determining spatial relationships between game elements

%% Distance Rules
%% Determines if two elements are within interaction range

%% Same Cell Distance Rule
%% Checks if an element is in the same cell as an object or another cell
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem2, [X2, Y2]], Type),
    (Type = object; Type = cell),
    same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

%% Adjacent Cell Distance Rule
%% Checks if an element is adjacent to a danger or another cell
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem2, [X2, Y2]], Type),
    (Type = danger; Type = cell),
    adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

%% Same Cell Check
%% Verifies if two different elements occupy the same coordinates
same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2,     % Elements must be different
    X1 is X2,           % X coordinates must match
    Y1 is Y2.           % Y coordinates must match

%% Adjacent Cell Check
%% Determines if two cells are directly adjacent (orthogonally connected)
%% Uses a list of delta positions to check all four directions
adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2,
    Delta = [[0,1], [0,-1], [1,0], [-1,0]],  % Up, Down, Right, Left
    member([DX,DY], Delta),
    X1 is X2 + DX,
    Y1 is Y2 + DY.