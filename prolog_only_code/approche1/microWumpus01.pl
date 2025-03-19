:- use_module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

:- dynamic (wumpus/0) as (incremental).
:- dynamic (or/0) as (incremental).
:- dynamic (peureux/0) as (incremental).
:- dynamic (cupide/0) as (incremental).

% Règles de prise de décision
% modélisées par l'ingénieur des connaissances
% Signification
%
% wumpus : vrai s'il y a un wumpus dans la case où est l'agent, faux sinon
% de même avec les éléments de situation gouffre et or
%
% peureux : vrai si l'agent a le trait de personnalité peureux
% de même avec les traits sportif, cupide, consciencieux

fuir :- wumpus, peureux.
combattre :- wumpus, \+peureux.
ramasser :- \+wumpus, or, cupide.
ramasser_apres_nettoyage_souterrain :- \+wumpus, or, \+cupide.
continuer_exploration :- \+wumpus, \+or.




% --------------------------- TESTS ---------------------------
% Affichage du résultat
res :- member(X,[fuir,combattre,ramasser,
    ramasser_apres_nettoyage_souterrain,
    continuer_exploration]),
    X,
    writeln(X),
    false.

res1 :- 
    Action = [fuir,combattre,ramasser,
    ramasser_apres_nettoyage_souterrain,
    continuer_exploration],
    Situation = [wumpus,or],
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


% Teste toutes les situations avec un élément de situation
% et un trait de personnalité pour voir quelles sont
% les actions qui en découlent
res2 :- 

    Action = [fuir,combattre,ramasser,ramasser_apres_nettoyage_souterrain,continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    member(S,Situation),
    member(P,Personnalite),
    assertz(S),     
    assertz(P),   
    findall(A,(member(A,Action),A),LA),    
    (LA=[]
    ->
        write([S]),write([P]),writeln("Pas d'action")
    ;
        write([S]),write([P]),writeln(LA)
    ),
    retract(S),
    retract(P),
    false.
    


% Teste toutes les situations avec un élément de situation
% et un trait de personnalité pour voir quelles sont
% les actions qui en découlent
% De plus affiche la liste et le nombre de tous les faits vrais
% dans le modèle en cour pour chaque cas.
res3 :- 
    Action = [fuir,combattre,ramasser,ramasser_apres_nettoyage_souterrain,continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    member(S,Situation),
    member(P,Personnalite),
    assertz(S),      
    assertz(P),   
    findall(A,(member(A,Action),A),LA),   
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
    PossibleAtoms = [fuir,combattre,ramasser,
    ramasser_apres_nettoyage_souterrain,
    continuer_exploration,wumpus,or,peureux,cupide,situation_non_precisee,personnalite_non_precisee],
    findall(Atom,
        (member(Atom,PossibleAtoms),
        Atom),
        ListGroundAtoms).
    
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

/*
2 ?- res1.
[][]continuer_exploration
[][cupide]continuer_exploration
[][peureux]continuer_exploration
[][peureux,cupide]continuer_exploration
[or][]ramasser_apres_nettoyage_souterrain
[or][cupide]ramasser
[or][peureux]ramasser_apres_nettoyage_souterrain
[or][peureux,cupide]ramasser
[wumpus][]combattre
[wumpus][cupide]combattre
[wumpus][peureux]fuir
[wumpus][peureux,cupide]fuir
[wumpus,or][]combattre
[wumpus,or][cupide]combattre
[wumpus,or][peureux]fuir
[wumpus,or][peureux,cupide]fuir
true 
*/

/*
2 ?- res3.
[situation_non_precisee][personnalite_non_precisee][continuer_exploration]
Liste des faits vrais dans le modele :
    continuer_exploration
    situation_non_precisee
    personnalite_non_precisee
Nombre de faits vrais : 3

[situation_non_precisee][peureux][continuer_exploration]
Liste des faits vrais dans le modele :
    continuer_exploration
    peureux
    situation_non_precisee
Nombre de faits vrais : 3

[situation_non_precisee][cupide][continuer_exploration]
Liste des faits vrais dans le modele :
    continuer_exploration
    cupide
    situation_non_precisee
Nombre de faits vrais : 3

[wumpus][personnalite_non_precisee][combattre]
Liste des faits vrais dans le modele :
    combattre
    wumpus
    personnalite_non_precisee
Nombre de faits vrais : 3

[wumpus][peureux][fuir]
Liste des faits vrais dans le modele :
    fuir
    wumpus
    peureux
Nombre de faits vrais : 3

[wumpus][cupide][combattre]
Liste des faits vrais dans le modele :
    combattre
    wumpus
    cupide
Nombre de faits vrais : 3

[or][personnalite_non_precisee][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modele :
    ramasser_apres_nettoyage_souterrain
    or
    personnalite_non_precisee
Nombre de faits vrais : 3

[or][peureux][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modele :
    ramasser_apres_nettoyage_souterrain
    or
    peureux
Nombre de faits vrais : 3

[or][cupide][ramasser]
Liste des faits vrais dans le modele :
    ramasser
    or
    cupide
Nombre de faits vrais : 3

false.

3 ?-
*/
