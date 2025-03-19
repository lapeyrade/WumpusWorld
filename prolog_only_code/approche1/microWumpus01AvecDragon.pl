
:- use_module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

:- dynamic (wumpus/0) as (incremental).
:- dynamic (dragon/0) as (incremental).
:- dynamic (or/0) as (incremental).
:- dynamic (peureux/0) as (incremental).
:- dynamic (cupide/0) as (incremental).

% Règles de prise de décision
% modélisées par l'ingénieur des connaissances

fuir :- wumpus, peureux.
fuir :- \+wumpus, dragon, peureux.
combattre :- wumpus, \+peureux.
discuter :- \+wumpus, dragon, \+peureux.
ramasser :- \+wumpus, \+dragon, or, cupide.
ramasser_apres_nettoyage_souterrain :- \+wumpus, \+dragon, or, \+cupide.
continuer_exploration :- \+wumpus, \+dragon, \+or.



% Affichage du résultat
res :- member(X,[fuir,combattre,ramasser,
    ramasser_apres_nettoyage_souterrain,discuter,
    continuer_exploration]),
    X,
    writeln(X),
    false.

res1 :- 
    Action = [fuir,combattre,ramasser,discuter,
    ramasser_apres_nettoyage_souterrain,
    continuer_exploration],
    Situation = [wumpus,or,dragon],
    Personnalite = [peureux,cupide],
    findall(S,subseq(Situation,S,_),ListeSituation),
    findall(P,subseq(Personnalite,P,_),ListePersonnalite),
    res11(ListeSituation,ListePersonnalite,Action,ListePersonnalite,Action).

res11([],_,_,_,_).
res11([_|TS],[],_,LP,LA):-
    res11(TS,LP,LA,LP,LA).
res11([HS|TS],[_|TP],[],LP,LA):-
    res11([HS|TS],TP,LA,LP,LA).
res11([HS|TS],[HP|TP],[HA|TA],LP,LA):-
    assertzList(HS),
    assertzList(HP),
    (call(HA)
    ->
        write(HS),write(HP),writeln(HA)
    ;
        true
    ),
    retractList(HS),
    retractList(HP),
    res11([HS|TS],[HP|TP],TA,LP,LA).

assertzList([]).
assertzList([H|T]):-
    assertz(H),
    assertzList(T).

retractList([]).
retractList([H|T]):-
    retract(H),
    retractList(T).



% Espace combinatoire à 3 dimensions
% - dimension 1 : l'ensemble des ensembles d'éléments de situations
% - dimension 2 : l'ensemble des ensembles de traits de personnalité
% - dimension 3 : l'ensemble des actions
% Cette approche consiste à isoler des triplets dans cet espace à 3 dimensions.
% Chaque triplet est un cas particulier.
% Si on veut complexifier la prise de décision, il faut ajouter des cas.
% Exemple : si on veut dire qu'un explorateur peureux et cupide va tout de même
% combattre s'il est en présence d'un wumpus et d'or, alors 
% il faut ajouter une règle. Si on veut le positionner dans l'arbre de décision,
% il faut en plus chercher où insérer ce nouveau cas.




% Teste toutes les situations avec un élément de situation
% et un trait de personnalité pour voir quelles sont
% les actions qui en découlent
res2 :- 

    Action = [fuir,discuter,combattre,ramasser,ramasser_apres_nettoyage_souterrain,continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or,dragon],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    member(S,Situation),
    member(P,Personnalite),
    assertz(S),      %writeln(assertz(S)),
    assertz(P),   %writeln(assertz(P)),
    findall(A,(member(A,Action),A),LA),    %writeln(findall(A,(member(A,action),A),LA)),
    (LA=[]
    ->
        write([S]),write([P]),writeln("Pas d'action")
    ;
        write([S]),write([P]),writeln(LA)
    ),
    retract(S),
    retract(P),
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




% Teste toutes les situations avec un élément de situation
% et un trait de personnalité pour voir quelles sont
% les actions qui en découlent
% De plus affiche la liste et le nombre de tous les faits vrais
% dans le modèle en cour pour chaque cas.
res3 :- 
    Action = [fuir,discuter,combattre,ramasser,ramasser_apres_nettoyage_souterrain,continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or,dragon],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    member(S,Situation),
    member(P,Personnalite),
    assertz(S),      %writeln(assertz(S)),
    assertz(P),   %writeln(assertz(P)),
    findall(A,(member(A,Action),A),LA),    %writeln(findall(A,(member(A,action),A),LA)),
    (LA=[]
    ->
        write([S]),write([P]),writeln("Pas d'action")
    ;
        write([S]),write([P]),writeln(LA)
    ),
    groundAtomsArity0(ListGroundAtoms),
    writeln("Liste des faits vrais dans le modele :"),
    displayList(ListGroundAtoms,"    "),
    length(ListGroundAtoms,NbTrueFacts),
    write("Nombre de faits vrais : "), writeln(NbTrueFacts),nl,
    retract(S),
    retract(P),
    false.




% Renvoie la liste des faits vrais pour tous les prédicats 
% de l'exemple
groundAtomsArity0(ListGroundAtoms) :-
    PossibleAtoms = [fuir,discuter,combattre,ramasser,
    ramasser_apres_nettoyage_souterrain,
    continuer_exploration,dragon,wumpus,or,peureux,cupide,situation_non_precisee,personnalite_non_precisee],
    findall(Atom,
        (member(Atom,PossibleAtoms),
        Atom),
        ListGroundAtoms).