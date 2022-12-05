% :- module(location, [location/3]).

:- multifile [situation/2].

%%%%% ONTOLOGY LOCATION %%%%%
% LOCATION: SAME_CELL, RIGHT_ROW, LEFT_ROW, UP_COL, DOWN_COL

% Elem1 is in the same cell as Elem2
same_cell(Id1, Id2):-
    situation(Id1, [X, Y]),
    situation(Id2, [X, Y]).

% X is to the right of Y
right_col(Id1, Id2):-
    situation(Id1, [X1, Y]),
    situation(Id2, [X2, Y]),
    X1 > X2.

% X is to the left of Y 
left_col(Id1, Id2):-
    situation(Id1, [X1, Y]),
    situation(Id2, [X2, Y]),
    X1 < X2.

% X is above Y 
up_row(Id1, Id2):-
    situation(Id1, [X, Y1]),
    situation(Id2, [X, Y2]),
    Y1 > Y2.

% X is below of Y 
down_row(Id1, Id2):-
    situation(Id1, [X, Y1]),
    situation(Id2, [X, Y2]),
    Y1 < Y2.

% X is in the right cell of Y
right_cell(Id1, Id2):-
    situation(Id1, [X1, Row]),
    situation(Id2, [X2, Row]),
    X1 is X2 + 1.

% X is in the left cell of Y
left_cell(Id1, Id2):-
    situation(Id1, [X1, Row]),
    situation(Id2, [X2, Row]),
    X1 is X2 - 1.

% X is in the upper cell of Y
up_cell(Id1, Id2):-
    situation(Id1, [X, Y1]),
    situation(Id2, [X, Y2]),
    Y1 is Y2 + 1.

% X is in the lower cell of Y
down_cell(Id1, Id2):-
    situation(Id1, [X, Y1]),
    situation(Id2, [X, Y2]),
    Y1 is Y2 - 1.

% X is in an adjacent cell of Y
adjacent_cell(Id1, Id2):-
    right_cell(Id1, Id2);
    left_cell(Id1, Id2);
    up_cell(Id1, Id2);
    down_cell(Id1, Id2).

location_(Id1, Id2):- same_cell(Id1, Id2).
location_(Id1, Id2):- right_col(Id1, Id2).
location_(Id1, Id2):- left_col(Id1, Id2).
location_(Id1, Id2):- up_row(Id1, Id2).
location_(Id1, Id2):- down_row(Id1, Id2).
location_(Id1, Id2):- right_cell(Id1, Id2).
location_(Id1, Id2):- left_cell(Id1, Id2).
location_(Id1, Id2):- up_cell(Id1, Id2).
location_(Id1, Id2):- down_cell(Id1, Id2).
location_(Id1, Id2):- adjacent_cell(Id1, Id2).

% Query Location
location(Id1, Id2, Location):-
    clause(location_(Id1, Id2), Loc),
    call(Loc),
    Loc =.. [Location, _, _].