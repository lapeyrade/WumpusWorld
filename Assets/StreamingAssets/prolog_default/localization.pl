%% Localization System - Spatial relationships and distances

:- discontiguous data_concept/2.

%% Distance predicates for element interactions

%% Check if elements are in the same cell
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem1, [X1, Y1]], human),
    data_concept([Elem, [X2, Y2]], Elem2),
    (
     data_concept([Elem, [X2, Y2]], object);
     data_concept([Elem, [X2, Y2]], cell)
    ),
    Elem1 \= Elem2,
    X1 is X2,
    Y1 is Y2.

%% Check if elements are in adjacent cells
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    data_concept([Elem1, [X1, Y1]], human),
    data_concept([Elem, [X2, Y2]], Elem2),
    (
     data_concept([Elem, [X2, Y2]], danger);
     data_concept([Elem, [X2, Y2]], cell)
    ),
    Elem1 \= Elem2,
    Delta = [[0,1], [0,-1], [1,0], [-1,0]],
    member([DX,DY], Delta),
    X1 is X2 + DX,
    Y1 is Y2 + DY.