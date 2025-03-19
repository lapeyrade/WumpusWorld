:- use_module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

:- discontiguous
    situation/1,
    personnalite/1.

:- dynamic (situation/1) as (incremental).
:- dynamic (personnalite/1) as (incremental).


action(A) :- 
    situation_personnalite(S,P), 
    spa(S,P,A).

situation_personnalite(S,P):-
    (situation(S) *-> true ; S = situation_non_precisee), 
    (personnalite(P) *-> true ; P = personnalite_non_precisee).
    
spa(wumpus,peureux,fuir).
spa(wumpus,P,combattre) :- personnalite(P), \+ personnalite(peureux).
spa(or,cupide,ramasser).
spa(or,P,ramasser_apres_nettoyage_souterrain) :- personnalite(P), \+ personnalite(cupide).
spa(situation_non_precisee,_,continuer_exploration).
 

    
    
    
% Teste toutes les situations avec un élément de situation
% et un trait de personnalité pour voir quelles sont
% les actions qui en découlent
res2 :- 
    Situation = [situation_non_precisee,wumpus,or],
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
    abolish_all_tables,
    false.

% En plus de res2, affiche la liste et le nombre de tous les faits vrais
% dans le modèle en cour pour chaque cas.
res3 :- 
    Situation = [situation_non_precisee,wumpus,or],
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

    groundAtoms(ListGroundAtoms),
    writeln("Liste des faits vrais dans le modele :"),
    displayList(ListGroundAtoms,"    "),
    length(ListGroundAtoms,NbTrueFacts),
    write("Nombre de faits vrais : "), writeln(NbTrueFacts),nl,

    retract(situation(S)),
    retract(personnalite(P)),
    abolish_all_tables,
    false.


% Renvoie la liste des faits vrais pour tous les prédicats 
% de l'exemple
groundAtoms(ListGroundAtoms) :-
    Preds = [situation/1, personnalite/1, situation_personnalite/2, spa/3, action/1],
    findall(Res,
        (member(Pred/Arity,Preds),
        length(Args, Arity),
        Atom =.. [Pred|Args],
        findall(Atom, Atom, Res)),
    ListListGroundAtoms),
    flatten(ListListGroundAtoms, ListGroundAtoms).

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
false.
*/

/*
2 ?- res3.
[situation_non_precisee][personnalite_non_precisee][continuer_exploration]
Liste des faits vrais dans le modÃ¨le :
    situation(situation_non_precisee)
    personnalite(personnalite_non_precisee)
    situation_personnalite(situation_non_precisee,personnalite_non_precisee)
    spa(wumpus,peureux,fuir)
    spa(wumpus,personnalite_non_precisee,combattre)
    spa(or,cupide,ramasser)
    spa(or,personnalite_non_precisee,ramasser_apres_nettoyage_souterrain)
    spa(situation_non_precisee,_2410,continuer_exploration)
    action(continuer_exploration)
Nombre de faits vrais : 9

[situation_non_precisee][peureux][continuer_exploration]
Liste des faits vrais dans le modÃ¨le :
    situation(situation_non_precisee)
    personnalite(peureux)
    situation_personnalite(situation_non_precisee,peureux)
    spa(wumpus,peureux,fuir)
    spa(or,peureux,ramasser_apres_nettoyage_souterrain)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2986,continuer_exploration)
    action(continuer_exploration)
Nombre de faits vrais : 8

[situation_non_precisee][cupide][continuer_exploration]
Liste des faits vrais dans le modÃ¨le :
    situation(situation_non_precisee)
    personnalite(cupide)
    situation_personnalite(situation_non_precisee,cupide)
    spa(wumpus,peureux,fuir)
    spa(wumpus,cupide,combattre)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2986,continuer_exploration)
    action(continuer_exploration)
Nombre de faits vrais : 8

[wumpus][personnalite_non_precisee][combattre]
Liste des faits vrais dans le modÃ¨le :
    situation(wumpus)
    personnalite(personnalite_non_precisee)
    situation_personnalite(wumpus,personnalite_non_precisee)
    spa(wumpus,peureux,fuir)
    spa(wumpus,personnalite_non_precisee,combattre)
    spa(or,cupide,ramasser)
    spa(or,personnalite_non_precisee,ramasser_apres_nettoyage_souterrain)
    spa(situation_non_precisee,_3002,continuer_exploration)
    action(combattre)
Nombre de faits vrais : 9

[wumpus][peureux][fuir]
Liste des faits vrais dans le modÃ¨le :
    situation(wumpus)
    personnalite(peureux)
    situation_personnalite(wumpus,peureux)
    spa(wumpus,peureux,fuir)
    spa(or,peureux,ramasser_apres_nettoyage_souterrain)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2988,continuer_exploration)
    action(fuir)
Nombre de faits vrais : 8

[wumpus][cupide][combattre]
Liste des faits vrais dans le modÃ¨le :
    situation(wumpus)
    personnalite(cupide)
    situation_personnalite(wumpus,cupide)
    spa(wumpus,peureux,fuir)
    spa(wumpus,cupide,combattre)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2988,continuer_exploration)
    action(combattre)
Nombre de faits vrais : 8

[or][personnalite_non_precisee][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modÃ¨le :
    situation(or)
    personnalite(personnalite_non_precisee)
    situation_personnalite(or,personnalite_non_precisee)
    spa(wumpus,peureux,fuir)
    spa(wumpus,personnalite_non_precisee,combattre)
    spa(or,cupide,ramasser)
    spa(or,personnalite_non_precisee,ramasser_apres_nettoyage_souterrain)
    spa(situation_non_precisee,_3002,continuer_exploration)
    action(ramasser_apres_nettoyage_souterrain)
Nombre de faits vrais : 9

[or][peureux][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modÃ¨le :
    situation(or)
    personnalite(peureux)
    situation_personnalite(or,peureux)
    spa(wumpus,peureux,fuir)
    spa(or,peureux,ramasser_apres_nettoyage_souterrain)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2988,continuer_exploration)
    action(ramasser_apres_nettoyage_souterrain)
Nombre de faits vrais : 8

[or][cupide][ramasser]
Liste des faits vrais dans le modÃ¨le :
    situation(or)
    personnalite(cupide)
    situation_personnalite(or,cupide)
    spa(wumpus,peureux,fuir)
    spa(wumpus,cupide,combattre)
    spa(or,cupide,ramasser)
    spa(situation_non_precisee,_2988,continuer_exploration)
    action(ramasser)
Nombre de faits vrais : 8

false.
*/