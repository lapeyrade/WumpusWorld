:- discontiguous
    situation/1,
    personnalite/1.

:- dynamic (situation/1) as (incremental).
:- dynamic (personnalite/1) as (incremental).


action(A) :- 
    situation_personnalite(S,P), 
    spa(S,P,A).

situation_personnalite(S,P):-
    (situation(S) -> true ; S = situation_non_precisee), 
    (personnalite(P) -> true ; P = personnalite_non_precisee).
    
spa(wumpus,peureux,fuir).
spa(wumpus,P,combattre) :- 
        personnalite(P), \+ personnalite(peureux).
spa(or,cupide,ramasser).
spa(or,P,ramasser_apres_nettoyage_souterrain) :- 
        personnalite(P), \+ personnalite(cupide).
spa(situation_non_precisee,_,continuer_exploration).
spa(dragon,peureux,fuir).
spa(dragon,P,discuter) :-
        personnalite(P), \+ personnalite(peureux).



res2 :- 
    Situation = [situation_non_precisee,wumpus,or,dragon],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    member(S,Situation),
    member(P,Personnalite),
    assertz(situation(S)),      %writeln(assertz(situation(S))),
    assertz(personnalite(P)),   %writeln(assertz(personnalite(P))),
    findall(A,action(A),LA),    %writeln(findall(A,action(A),LA)),
    (LA=[]
    ->
        write([S]),write([P]),writeln("Pas d'action")
    ;
        write([S]),write([P]),writeln(LA)
    ),
    retract(situation(S)),
    retract(personnalite(P)),
    false.
/*
2 ?- res2.
[situation_non_precisee][personnalite_non_precisee][continuer_exploration]
[situation_non_precisee][peureux][continuer_exploration]
[situation_non_precisee][cupide][continuer_exploration]
[wumpus][personnalite_non_precisee][combattre]
[wumpus][peureux][fuir]
[wumpus][cupide][combattre]
[or][personnalite_non_precisee][ramasser_apres_nettoyage_souterrain]
[or][peureux][ramasser_apres_nettoyage_souterrain]
[or][cupide][ramasser]
[dragon][personnalite_non_precisee][discuter]
[dragon][peureux][fuir]
[dragon][cupide][discuter]
false.
*/


