:- module(desire, [desire/2]).

:- multifile [personality:personality/2].

%%%%%%%%%% DESIRE ONTOLOGY %%%%%%%%%%
% DESIRE: GET_RICH, KILL_WUMPUS, EXPLORE

get_rich(X):-
    \+ personality:personality(X, nonmaterialistic).

kill_wumpus(X):-
    \+ personality:personality(X, pacifist).

explore_cave(_).

desire_(X) :- get_rich(X).
desire_(X) :- kill_wumpus(X).
desire_(X) :- explore_cave(X).

% QUERY Desire
desire(X, Desire):-
    clause(desire_(X), D),
    call(D),
    D =.. [Desire, _].

% most_wanted_desire(Desire, Max):-
%     findall(Weight, desire(X, Desire, Weight), Result),
%     max_member(Max, Result),
%     desire(X, Desire, Max).