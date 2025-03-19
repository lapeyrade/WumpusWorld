:- use_module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

:- discontiguous
    action/1,
    sousClasseDe/2,
    eprouve_emotion/3,
    emotion_action/2.

:- discontiguous
    situation/1,
    personnalite/1.

:- dynamic (situation/1) as (incremental).
:- dynamic (personnalite/1) as (incremental).


% Règles d'inférence de prise de décision
action(A) :- 
    situation_personnalite(S,P),
    eprouve_emotion(P,S,E),
    emotion_action(E,A).

situation_personnalite(S,P):-
    (situation(S) *-> true ; S = situation_non_precisee), 
    (personnalite(P) *-> true ; P = personnalite_non_precisee).

% Faits définissant le prédicat eprouve_emotion
eprouve_emotion(peureux,wumpus,peur).
eprouve_emotion(P,wumpus,agressivite) :-
    personnalite(P), \+ personnalite(peureux).
eprouve_emotion(cupide,or,envie).
eprouve_emotion(P,or,envie_et_devoir) :- 
    personnalite(P), \+ personnalite(cupide).
eprouve_emotion(P,situation_non_precisee,curiosite):- 
    personnalite(P).

% Faits définissant le prédicat emotion_action
emotion_action(peur,fuir).
emotion_action(agressivite,combattre).
emotion_action(envie,ramasser).
emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain).
emotion_action(curiosite,continuer_exploration).









res2 :- 
    % retractall(situation(_)),
    % retractall(personnalite(_)),
    % Action = [fuir,combattre,ramasser,
    % ramasser_apres_nettoyage_souterrain,
    % escalader,
    % appeler_secours,
    % continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or],
    Personnalite = [personnalite_non_precisee,peureux,cupide],
    % member(A,Action),
    member(S,Situation),
    member(P,Personnalite),
    assertz(situation(S)),
    assertz(personnalite(P)),
    % (action(A)
    % ->
    %     write([S]),write([P]),writeln(A)
    % ;
    %     true
    % ),
    findall(A,action(A),LA),
    (LA=[]
    ->
        write([S]),write([P]),writeln("Pas d'action")
    ;
        write([S]),write([P]),writeln(LA)
    ),
    retract(situation(S)),
    retract(personnalite(P)),
 %   abolish_all_tables,
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
false.
*/



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
    false.


% Renvoie la liste des faits vrais pour tous les prédicats 
% de l'exemple
groundAtoms(ListGroundAtoms) :-
    Preds = [situation/1, personnalite/1, situation_personnalite/2, action/1, eprouve_emotion/3, emotion_action/2, sousClasseDe/2],
    findall(Res,
        (member(Pred/Arity,Preds),
        length(Args, Arity),
        Atom =.. [Pred|Args],
        findall(Atom, Atom, Res)),
    ListListGroundAtoms),
    flatten(ListListGroundAtoms, ListGroundAtoms).


/*
3 ?- res3.
[situation_non_precisee][personnalite_non_precisee][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(situation_non_precisee)
    personnalite(personnalite_non_precisee)
    situation_personnalite(situation_non_precisee,personnalite_non_precisee)
    action(continuer_exploration)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 14

[situation_non_precisee][peureux][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(situation_non_precisee)
    personnalite(peureux)
    situation_personnalite(situation_non_precisee,peureux)
    action(continuer_exploration)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(peureux,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

[situation_non_precisee][cupide][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(situation_non_precisee)
    personnalite(cupide)
    situation_personnalite(situation_non_precisee,cupide)
    action(continuer_exploration)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

[wumpus][personnalite_non_precisee][combattre]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(personnalite_non_precisee)
    situation_personnalite(wumpus,personnalite_non_precisee)
    action(combattre)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 14

[wumpus][peureux][fuir]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(peureux)
    situation_personnalite(wumpus,peureux)
    action(fuir)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(peureux,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

[wumpus][cupide][combattre]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(cupide)
    situation_personnalite(wumpus,cupide)
    action(combattre)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

[or][personnalite_non_precisee][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(personnalite_non_precisee)
    situation_personnalite(or,personnalite_non_precisee)
    action(ramasser_apres_nettoyage_souterrain)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 14

[or][peureux][ramasser_apres_nettoyage_souterrain]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(peureux)
    situation_personnalite(or,peureux)
    action(ramasser_apres_nettoyage_souterrain)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(peureux,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

[or][cupide][ramasser]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(cupide)
    situation_personnalite(or,cupide)
    action(ramasser)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    emotion_action(curiosite,continuer_exploration)
Nombre de faits vrais : 13

false.
4 ?-
*/