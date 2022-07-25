:- use_module([action, alignment, cell, location, move, objective, personality, type, wellfs]).

% Dynamic predicate in main file if not already defined in used modules
:- dynamic([nb_arrow_shot/1, grid_coord/4], [incremental(true)]).

%     action_envisageable(X, Action),
% action(X, Action):-
%     action_possible(X, Actions),
%     meilleur_action(Actions).


% Action envisageable = shoot
% Action possible = shoot_left...


%% souhait agent => tuer wumpus
%% action souhaité => shoot
%% action proposé/réelle => shoot_left/move_back



% % comportement (face au danger pour les 3 premières)
% tuer_danger(X, Y):-
%     type(Y, etre_vivant),
%     personality(X, hunter).

% eviter_danger(X, _):-
%     personality(X, pacifist).

% desarmocer_danger(X, Y):-
%     type(Y, objet),
%     personality(X, bricoleur).


% % objective
% tuer_wumpus(X):
%     personality(X, hunter).

% % action envisageable
% shoot(X):-
%     situation(X, wumpus_detecte(Col, Row)),
%     objective(X, tuer_wumpus).

% % action possible
% shoot(X):-
%     location(X, wumpus_aligne(Col, Row)),
%     equipement(X, fleche_dispo(Arrow)).





% action_(X) :- shoot_envisageable(X).

% % action envisageable
% shoot_envisageable(X):-
%     situation(X, wumpus_detecte(Col, Row)),
%     objective(X, tuer_wumpus).


