:- module(location, [location/3]).

:- multifile [situation:situation/2].

%%%%% ONTOLOGY LOCATION %%%%%
% LOCATION: SAME_CELL, RIGHT_ROW, LEFT_ROW, UP_COL, DOWN_COL

% X is in the same cell as Y
same_cell(X, Y):-
    situation:situation([Col, Row], X),
    situation:situation([Col, Row], Y).

% X is to the right of Y
right_col(X, Y):-
    situation:situation([ColX, Row], X),
    situation:situation([ColY, Row], Y),
    ColX > ColY.

% X is to the left of Y 
left_col(X, Y):-
    situation:situation([ColX, Row], X),
    situation:situation([ColY, Row], Y),
    ColX < ColY.

% X is above Y 
up_row(X, Y):-
    situation:situation([Col, RowX], X),
    situation:situation([Col, RowY], Y),
    RowX > RowY.

% X is below of Y 
down_row(X, Y):-
    situation:situation([Col, RowX], X),
    situation:situation([Col, RowY], Y),
    RowX < RowY.

% X is in the right cell of Y
right_cell(X, Y):-
    situation:situation([ColX, Row], X),
    situation:situation([ColY, Row], Y),
    ColX is ColY + 1.

% X is in the left cell of Y
left_cell(X, Y):-
    situation:situation([ColX, Row], X),
    situation:situation([ColY, Row], Y),
    ColX is ColY - 1.

% X is in the upper cell of Y
up_cell(X, Y):-
    situation:situation([Col, RowX], X),
    situation:situation([Col, RowY], Y),
    RowX is RowY + 1.

% X is in the lower cell of Y
down_cell(X, Y):-
    situation:situation([Col, RowX], X),
    situation:situation([Col, RowY], Y),
    RowX is RowY - 1.

% X is in an adjacent cell of Y
adjacent_cell(X, Y):-
    right_cell(X, Y); left_cell(X, Y);
    up_cell(X, Y); down_cell(X, Y).

location_(X, Y):- same_cell(X, Y).
location_(X, Y):- right_col(X, Y).
location_(X, Y):- left_col(X, Y).
location_(X, Y):- up_row(X, Y).
location_(X, Y):- down_row(X, Y).
location_(X, Y):- right_cell(X, Y).
location_(X, Y):- left_cell(X, Y).
location_(X, Y):- up_cell(X, Y).
location_(X, Y):- down_cell(X, Y).
location_(X, Y):- adjacent_cell(X, Y).

% Query Location
location(X, Y, Location):-
    clause(location_(X, Y), Loc),
    call(Loc),
    Loc =.. [Location, _, _].