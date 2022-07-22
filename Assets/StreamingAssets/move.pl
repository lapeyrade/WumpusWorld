:- module(move, [move/2, random_move/2]).

:- use_module(library(random)).

%%%%%%%%%% MOVE ONTOLOGY %%%%%%%%%%

% all golds are found by the agent
all_golds_found :-
    nb_gold(TotalGold),
    nb_gold_agent(AgentGold),
    TotalGold == AgentGold.

% all wumpus are killed by the agent
all_wumpus_killed :-
            nb_wumpus(TotalWumpus),
            nb_wumpus_dead(WumpusDead),
            TotalWumpus == WumpusDead.

% cell content is undefined
unvisited_safe_cell(Col, Row):-
    \+ cell2(Col, Row, visited),
    \+ cell2(Col, Row, wall),
    cell2(Col, Row, safe).

% cell content is undefined
undefined_cell(Col, Row):-
    \+ cell2(Col, Row, visited),
    \+ cell2(Col, Row, wall),
    is_undefined(cell2(Col, Row, wumpus)),
    is_undefined(cell2(Col, Row, pit)).

% X move to the last cell visited
move_back(X):-
    \+ objective(X, explore_all),
    (
        (
            \+ objective(X, gold), nb_gold_agent(1)
        ),
        (
            all_golds_found
        )
    ),
    ( 
        \+ objective(X, kill)
        ;
        ( objective(X, kill), all_wumpus_killed )
    ), !.

% X move to the last cell visited
move_back(X):-
    \+ move_right(X),
    \+ move_left(X),
    \+ move_up(X),
    \+ move_down(X).

% X move to the right cell
move_right(X):-
    cell2(Col, Row, X),
    RightCol is Col+1,
    unvisited_safe_cell(RightCol, Row).

% X move to the left cell
move_left(X):-
    cell2(Col, Row, X),
    LeftCol is Col-1,
    unvisited_safe_cell(LeftCol, Row).

% X move to the top cell
move_up(X):-
    cell2(Col, Row, X),
    UpRow is Row+1,
    unvisited_safe_cell(Col, UpRow).

% X move to the bottom cell
move_down(X):-
    cell2(Col, Row, X),
    DownRow is Row-1,
    unvisited_safe_cell(Col, DownRow).

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
    objective(X, determinist_exploration),
    clause(move_(X), M),
    call(M),
    M =.. [Move, _].

% QUERY Random Move
move(X, Move):-
    objective(X, stochastic_exploration),
    random_move(X, Move).