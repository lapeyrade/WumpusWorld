:- module(move, [move/2, random_move/2]).

:- use_module(library(random)).

:- multifile [situation:situation/2, objective:objective/2, state:state/2].

%%%%%%%%%% MOVE ONTOLOGY %%%%%%%%%%

% X move to the last cell visited
move_back(X):-
    \+ objective:objective(X, explore_cave),
    (
        \+ objective:objective(X, hunt_wumpus);
        (
            objective:objective(X, hunt_wumpus),
            \+ state:state(X, can_shoot_arrow)
        )
    ),
    (
        objective:objective(X, find_gold),
        state:state(X, gold_found)
    ), !.

% X move to the last cell visited
move_back(X):-
    \+ move_right(X),
    \+ move_left(X),
    \+ move_up(X),
    \+ move_down(X).

% X move to the right cell
move_right(X):-
    situation:situation([Col, Row], X),
    RightCol is Col+1,
    situation:situation([RightCol, Row], unvisited_safe_cell).

% X move to the left cell
move_left(X):-
    situation:situation([Col, Row], X),
    LeftCol is Col-1,
    situation:situation([LeftCol, Row], unvisited_safe_cell).

% X move to the top cell
move_up(X):-
    situation:situation([Col, Row], X),
    UpRow is Row+1,
    situation:situation([Col, UpRow], unvisited_safe_cell).

% X move to the bottom cell
move_down(X):-
    situation:situation([Col, Row], X),
    DownRow is Row-1,
    situation:situation([Col, DownRow], unvisited_safe_cell).

% move randomly to a side cell
random_move(X, Move):-
    random_permutation([move_right, move_left, move_up, move_down, move_back], ListDirection),
    member(Move, ListDirection),
    call(Move, X).

move_(X):- move_back(X).
move_(X):- move_right(X).
move_(X):- move_left(X).
move_(X):- move_up(X).
move_(X):- move_down(X).

% QUERY Move
move(X, Move):-
    objective:objective(X, determinist_exploration),
    clause(move_(X), M),
    call(M),
    M =.. [Move, _].

% QUERY Random Move
move(X, Move):-
    objective:objective(X, stochastic_exploration),
    objective:
    random_move(X, Move).


% cell content is undefined
% unvisited_safe_cell(Col, Row):-
    % \+ situation:situation([Col, Row], visited),
    % \+ situation:situation([Col, Row], wall),
    % situation:situation([Col, Row], safe).

% % cell content is undefined
% undefined_cell(Col, Row):-
%     \+ situation:situation([Col, Row], visited),
%     \+ situation:situation([Col, Row], wall),
%     wellfs:is_undefined(cell2([Col, Row], wumpus)),
%     wellfs:is_undefined(cell2([Col, Row], pit)).