:- discontiguous data_concept/2.

/*** Localization rules***/
% Combined object/cell check for efficiency
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem2, [X2, Y2]], Type),
    (Type = object; Type = cell),
    same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

% Combined danger/cell check for efficiency
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem2, [X2, Y2]], Type),
    (Type = danger; Type = cell),
    adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2, 
    X1 is X2, 
    Y1 is Y2.

% Optimized adjacent_cell using delta positions
adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2,
    Delta = [[0,1], [0,-1], [1,0], [-1,0]],
    member([DX,DY], Delta),
    X1 is X2 + DX,
    Y1 is Y2 + DY.