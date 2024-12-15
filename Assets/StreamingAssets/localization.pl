:- discontiguous data_concept/2.

/*** Localization rules***/
distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    (
        data_concept([Elem2, [X2, Y2]], object);
        data_concept([Elem2, [X2, Y2]], cell)
    ),
    same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).

distance([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    (
        data_concept([Elem2, [X2, Y2]], danger);
        data_concept([Elem2, [X2, Y2]], cell)
    ),
    adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]).


same_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2, X1 is X2, Y1 is Y2.


adjacent_cell([Elem1, [X1, Y1]], [Elem2, [X2, Y2]]):-
    Elem1 \= Elem2,
    (
        X1 is X2, Y1 is Y2 + 1;
        X1 is X2, Y1 is Y2 - 1;
        X1 is X2 + 1, Y1 is Y2;
        X1 is X2 - 1, Y1 is Y2
    ).