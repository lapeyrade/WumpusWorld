:- module(move, [move/2, random_move/2]).

:- use_module(library(random)).

%%%%%%%%%% MOVE ONTOLOGY %%%%%%%%%%
all_goal_found :-
    nb_gold(TotalGold),
    nb_gold_agent(AgentGold),
    TotalGold == AgentGold.

all_wumpus_killed :-
            nb_wumpus(TotalWumpus),
            nb_wumpus_dead(WumpusDead),
            TotalWumpus == WumpusDead.

unvisited_safe_cell(Col, Row):-
    \+ cell2(Col, Row, visited),
    \+ cell2(Col, Row, wall),
    cell2(Col, Row, safe).

move_back(X):-
    all_goal_found,
    ( 
        \+ personality(X, hunter)
        ;
        ( personality(X, hunter), all_wumpus_killed )
    ), !.

move_back(X):-
    \+ move_right(X),
    \+ move_left(X),
    \+ move_up(X),
    \+ move_down(X).

move_right(X):-
    cell2(Col, Row, X),
    RightCol is Col+1,
    unvisited_safe_cell(RightCol, Row).

move_left(X):-
    cell2(Col, Row, X),
    LeftCol is Col-1,
    unvisited_safe_cell(LeftCol, Row).

move_up(X):-
    cell2(Col, Row, X),
    UpRow is Row+1,
    unvisited_safe_cell(Col, UpRow).


move_down(X):-
    cell2(Col, Row, X),
    DownRow is Row-1,
    unvisited_safe_cell(Col, DownRow).

% Move randomly to a side cell
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
    personality(X, determinist),
    clause(move_(X), M),
    call(M),
    M =.. [Move, _].

% QUERY Random Move
move(X, Move):-
    personality(X, stochastic),
    random_move(X, Move).




% next_move(Move):-
%     nb_gold(TotalGold), nb_gold_agent(AgentGold),
%     TotalGold == AgentGold,
%     ( 
%         \+(personality(agent, hunter))
%         ;
%         (
%             personality(agent, hunter),
%             nb_wumpus(TotalWumpus),
%             nb_wumpus_dead(WumpusDead),
%             TotalWumpus == WumpusDead
%         )
%     ),
%     Move = "move_back", !.

% next_move(Move):-
%     personality(agent, stochastic),
%     random_permutation([1, 2, 3, 4], ListDirection),
%     ListDirection = [_, _, _, _],
%     member(X, ListDirection),
%     move(X, Move).

% next_move(Move):-
%     personality(agent, determinist),
%     cell2(Col, Row, agent),
%     RightCol is Col+1,
%     \+(cell2(RightCol, Row, visited)),
%     \+(cell2(RightCol, Row, wall)),
%     cell2(RightCol, Row, safe),
%     Move = "move_right", !.

% next_move(Move):-
%     personality(agent, determinist),
%     cell2(Col, Row, agent),
%     LeftCol is Col-1,
%     \+(cell2(LeftCol, Row, visited)),
%     \+(cell2(LeftCol, Row, wall)),
%     cell2(LeftCol, Row, safe),
%     Move = "move_left", !.

% next_move(Move):-
%     personality(agent, determinist),
%     cell2(Col, Row, agent),
%     UpRow is Row+1,
%     \+(cell2(Col, UpRow, visited)),
%     \+(cell2(Col, UpRow, wall)),
%     cell2(Col, UpRow, safe),
%     Move = "move_up", !.

% next_move(Move):-
%     personality(agent, determinist),
%     cell2(Col, Row, agent),
%     DownRow is Row-1,
%     \+(cell2(Col, DownRow, visited)),
%     \+(cell2(Col, DownRow, wall)),
%     cell2(Col, DownRow, safe),
%     Move = "move_down", !.

% next_move(Move):-
%     Move = "move_back", !.

% move(1, Move):-
%     cell2(Col, Row, agent),
%     RightCol is Col+1,
%     \+(cell2(RightCol, Row, visited)),
%     \+(cell2(RightCol, Row, wall)),
%     cell2(RightCol, Row, safe),
%     Move = "move_right".

% move(2, Move):-
%     cell2(Col, Row, agent),
%     LeftCol is Col-1,
%     \+(cell2(LeftCol, Row, visited)),
%     \+(cell2(LeftCol, Row, wall)),
%     cell2(LeftCol, Row, safe),
%     Move = "move_left".

% move(3, Move):-
%     cell2(Col, Row, agent),
%     UpRow is Row+1,
%     \+(cell2(Col, UpRow, visited)),
%     \+(cell2(Col, UpRow, wall)),
%     cell2(Col, UpRow, safe),
%     Move = "move_up".

% move(4, Move):-
%     cell2(Col, Row, agent),
%     DownRow is Row-1,
%     \+(cell2(Col, DownRow, visited)),
%     \+(cell2(Col, DownRow, wall)),
%     cell2(Col, DownRow, safe),
%     Move = "move_down".

% %%%% RANDOM MOVEMENT %%%%
% % Move randomly to a side cell
% random_move(Move):-
%     random_between(1, 4, Random),
%     move_random(Random, Move).

% move_random(1, Move):-
%     Move = "move_right".

% move_random(2, Move):-
%     Move = "move_left".

% move_random(3, Move):-
%     Move = "move_up".

% move_random(4, Move):-
%     Move = "move_down".