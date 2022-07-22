:- module(location, [location/3]).

%%%%% ONTOLOGY LOCATION %%%%%
% LOCATION: SAME_CELL, RIGHT_ROW, LEFT_ROW, UP_COL, DOWN_COL

same_cell(X, Y):-
    cell2(Col, Row, X),
    cell2(Col, Row, Y).

right_col(X, Y):-
    cell2(ColX, Row, X),
    cell2(ColY, Row, Y),
    ColX > ColY.
    
left_col(X, Y):-
    cell2(ColX, Row, X),
    cell2(ColY, Row, Y),
    ColX < ColY.

up_row(X, Y):-
    cell2(Col, RowX, X),
    cell2(Col, RowY, Y),
    RowX > RowY.

down_row(X, Y):-
    cell2(Col, RowX, X),
    cell2(Col, RowY, Y),
    RowX < RowY.

location_(X, Y):- same_cell(X, Y).
location_(X, Y):- right_col(X, Y).
location_(X, Y):- left_col(X, Y).
location_(X, Y):- up_row(X, Y).
location_(X, Y):- down_row(X, Y).

% Query Location
location(X, Y, Location):-
    clause(location_(X, Y), Loc),
    call(Loc),
    Loc =.. [Location, _, _].