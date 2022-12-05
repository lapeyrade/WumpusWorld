% :- module(move, [move/2, random_move/2]).

:- use_module(library(random)).

:- multifile [situation/2, objective/2, state/2].

%%%%%%%%%% MOVE ONTOLOGY %%%%%%%%%%

% X move to the last cell visited
move_back(Id):-
    \+ objective(Id, explore_cave),
    (
        \+ objective(Id, hunt_wumpus);
        (
            objective(Id, hunt_wumpus),
            \+ state(Id, can_shoot_arrow)
        )
    ),
    (
        objective(Id, find_gold),
        state(Id, gold_found)
    ), !.

% X move to the last cell visited
move_back(Id):-
    \+ move_right(Id),
    \+ move_left(Id),
    \+ move_up(Id),
    \+ move_down(Id).

% X move to the right cell
move_right(Id):-
    situation(Id, [X, Y]),
    RightX is X+1,
    situation(unvisited_safe_cell, [RightX, Y]).

% X move to the left cell
move_left(Id):-
    situation(Id, [X, Y]),
    LeftX is X-1,
    situation(unvisited_safe_cell, [LeftX, Y]).

% X move to the top cell
move_up(Id):-
    situation(Id, [X, Y]),
    UpY is Y+1,
    situation(unvisited_safe_cell, [X, UpY]).

% X move to the bottom cell
move_down(Id):-
    situation(Id, [X, Y]),
    DownY is Y-1,
    situation(unvisited_safe_cell, [X, DownY]).

% move randomly to a side cell
random_move(Id, Move):-
    random_permutation([move_right, move_left, move_up, move_down, move_back], ListDirection),
    member(Move, ListDirection),
    call(Move, Id).

move_(Id):- move_back(Id).
move_(Id):- move_right(Id).
move_(Id):- move_left(Id).
move_(Id):- move_up(Id).
move_(Id):- move_down(Id).

% QUERY Move
move(Id, Move):-
    objective(Id, determinist_exploration),
    clause(move_(Id), M),
    call(M),
    M =.. [Move, _].

% QUERY Random Move
move(Id, Move):-
    objective(Id, stochastic_exploration),
    random_move(Id, Move).


% cell content is undefined
% unvisited_safe_cell(Col, Row):-
    % \+ situation([Col, Row], visited),
    % \+ situation([Col, Row], wall),
    % situation([Col, Row], safe).

% % cell content is undefined
% undefined_cell(Col, Row):-
%     \+ situation([Col, Row], visited),
%     \+ situation([Col, Row], wall),
%     wellfs:is_undefined(cell2([Col, Row], wumpus)),
%     wellfs:is_undefined(cell2([Col, Row], pit)).