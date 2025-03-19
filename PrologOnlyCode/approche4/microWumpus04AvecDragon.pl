:- use_module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

:- table 
    action/1,
    sousClasseDe/2,
    eprouve_emotion/3,
    emotion_action/2.

:- table 
    (
        situation/1,
        personnalite/1
    )
    as (incremental,dynamic).

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
eprouve_emotion(peureux,monstre,peur).
eprouve_emotion(P,monstre,agressivite) :-
    sousClasseDe(P,top_personnalite), \+ personnalite(peureux).
eprouve_emotion(cupide,objetDeValeur,envie).
eprouve_emotion(P,objetDeValeur,envie_et_devoir) :- 
    sousClasseDe(P,top_personnalite), \+ personnalite(cupide).
eprouve_emotion(P,situation_non_precisee,curiosite) :- 
    sousClasseDe(P,top_personnalite).

% Cas des dragons
sousClasseDe(dragon,monstre).
eprouve_emotion(P,dragon,curiosite_craintive) :-
    sousClasseDe(P,top_personnalite), \+ personnalite(peureux).

emotion_action(curiosite_craintive,discuter).

% Règles de spécialisation/généralisation de eprouve_emotion
eprouve_emotion(SousPerso,Sit,Emo):-
        eprouve_emotion(Perso,Sit,Emo),
        sousClasseDe(SousPerso,Perso).

eprouve_emotion(Perso,SousSit,Emo):-
        eprouve_emotion(Perso,Sit,Emo),
        sousClasseDe(SousSit,Sit).
    
eprouve_emotion(Perso,Sit,SuperEmo):-
        eprouve_emotion(Perso,Sit,Emo),
        sousClasseDe(Emo,SuperEmo).

% Faits emotion_action
emotion_action(peur,fuir).
emotion_action(agressivite,combattre). % :- \+situation(dragon).
emotion_action(envie,ramasser).
emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain).
emotion_action(curiosite,continuer_exploration).

% Règles de spécialisation/généralisation de emotion_action
emotion_action(SousEmotion,Action):-
        emotion_action(Emotion,Action),
        sousClasseDe(SousEmotion,Emotion).

% Reflexivité et transitivité du prédicat sousClasseDe
sousClasseDe(C,C).
sousClasseDe(C1,C2):-
    sousClasseDe(C1,C3),
    sousClasseDe(C3,C2).

% Hiérarchie des éléments de situation
sousClasseDe(danger,top_situation).
sousClasseDe(monstre,danger).
sousClasseDe(wumpus,monstre).
sousClasseDe(objetDeValeur,top_situation).
sousClasseDe(or,objetDeValeur).
sousClasseDe(situation_non_precisee,top_situation).

% Hiérarchie des traits de personnalité
sousClasseDe(cupide,top_personnalite).
sousClasseDe(peureux,top_personnalite).
sousClasseDe(personnalite_non_precisee,top_personnalite).

% Hiérarchie des émotions
sousClasseDe(envie,top_emotion).
sousClasseDe(peur,top_emotion).
sousClasseDe(agressivite,top_emotion).
sousClasseDe(curiosite,top_emotion).
sousClasseDe(envie_et_devoir,top_emotion).












res2 :- 
    % retractall(situation(_)),
    % retractall(personnalite(_)),
    % Action = [fuir,combattre,ramasser,
    % ramasser_apres_nettoyage_souterrain,
    % escalader,
    % appeler_secours,
    % continuer_exploration],
    Situation = [situation_non_precisee,wumpus,or,dragon],
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
    abolish_all_tables,
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
[dragon][personnalite_non_precisee][combattre,discuter]
[dragon][peureux][fuir]
[dragon][cupide][combattre,discuter]
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
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_6312,_6312)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[situation_non_precisee][peureux][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(situation_non_precisee)
    personnalite(peureux)
    situation_personnalite(situation_non_precisee,peureux)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8002,_8002)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[situation_non_precisee][cupide][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(situation_non_precisee)
    personnalite(cupide)
    situation_personnalite(situation_non_precisee,cupide)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8002,_8002)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[wumpus][personnalite_non_precisee][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(personnalite_non_precisee)
    situation_personnalite(wumpus,personnalite_non_precisee)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[wumpus][peureux][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(peureux)
    situation_personnalite(wumpus,peureux)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[wumpus][cupide][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(wumpus)
    personnalite(cupide)
    situation_personnalite(wumpus,cupide)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[or][personnalite_non_precisee][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(personnalite_non_precisee)
    situation_personnalite(or,personnalite_non_precisee)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[or][peureux][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(peureux)
    situation_personnalite(or,peureux)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

[or][cupide][continuer_exploration]
Liste des faits vrais dans le modele :
    situation(or)
    personnalite(cupide)
    situation_personnalite(or,cupide)
    action(continuer_exploration)
    eprouve_emotion(cupide,situation_non_precisee,curiosite)
    eprouve_emotion(cupide,situation_non_precisee,top_emotion)
    eprouve_emotion(cupide,monstre,top_emotion)
    eprouve_emotion(cupide,monstre,agressivite)
    eprouve_emotion(cupide,objetDeValeur,top_emotion)
    eprouve_emotion(cupide,objetDeValeur,envie)
    eprouve_emotion(cupide,objetDeValeur,envie_et_devoir)
    eprouve_emotion(cupide,wumpus,top_emotion)
    eprouve_emotion(cupide,wumpus,agressivite)
    eprouve_emotion(cupide,dragon,top_emotion)
    eprouve_emotion(cupide,dragon,curiosite_craintive)
    eprouve_emotion(cupide,dragon,agressivite)
    eprouve_emotion(cupide,or,top_emotion)
    eprouve_emotion(cupide,or,envie)
    eprouve_emotion(cupide,or,envie_et_devoir)
    eprouve_emotion(top_personnalite,situation_non_precisee,curiosite)
    eprouve_emotion(top_personnalite,situation_non_precisee,top_emotion)
    eprouve_emotion(top_personnalite,monstre,top_emotion)
    eprouve_emotion(top_personnalite,monstre,agressivite)
    eprouve_emotion(top_personnalite,objetDeValeur,top_emotion)
    eprouve_emotion(top_personnalite,objetDeValeur,envie_et_devoir)
    eprouve_emotion(top_personnalite,wumpus,top_emotion)
    eprouve_emotion(top_personnalite,wumpus,agressivite)
    eprouve_emotion(top_personnalite,dragon,top_emotion)
    eprouve_emotion(top_personnalite,dragon,curiosite_craintive)
    eprouve_emotion(top_personnalite,dragon,agressivite)
    eprouve_emotion(top_personnalite,or,top_emotion)
    eprouve_emotion(top_personnalite,or,envie_et_devoir)
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,curiosite)   
    eprouve_emotion(personnalite_non_precisee,situation_non_precisee,top_emotion) 
    eprouve_emotion(personnalite_non_precisee,monstre,top_emotion)
    eprouve_emotion(personnalite_non_precisee,monstre,agressivite)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,top_emotion)
    eprouve_emotion(personnalite_non_precisee,objetDeValeur,envie_et_devoir)      
    eprouve_emotion(personnalite_non_precisee,wumpus,top_emotion)
    eprouve_emotion(personnalite_non_precisee,wumpus,agressivite)
    eprouve_emotion(personnalite_non_precisee,dragon,top_emotion)
    eprouve_emotion(personnalite_non_precisee,dragon,curiosite_craintive)
    eprouve_emotion(personnalite_non_precisee,dragon,agressivite)
    eprouve_emotion(personnalite_non_precisee,or,top_emotion)
    eprouve_emotion(personnalite_non_precisee,or,envie_et_devoir)
    eprouve_emotion(peureux,situation_non_precisee,curiosite)
    eprouve_emotion(peureux,situation_non_precisee,top_emotion)
    eprouve_emotion(peureux,monstre,top_emotion)
    eprouve_emotion(peureux,monstre,peur)
    eprouve_emotion(peureux,monstre,agressivite)
    eprouve_emotion(peureux,objetDeValeur,top_emotion)
    eprouve_emotion(peureux,objetDeValeur,envie_et_devoir)
    eprouve_emotion(peureux,wumpus,top_emotion)
    eprouve_emotion(peureux,wumpus,peur)
    eprouve_emotion(peureux,wumpus,agressivite)
    eprouve_emotion(peureux,dragon,top_emotion)
    eprouve_emotion(peureux,dragon,peur)
    eprouve_emotion(peureux,dragon,curiosite_craintive)
    eprouve_emotion(peureux,dragon,agressivite)
    eprouve_emotion(peureux,or,top_emotion)
    eprouve_emotion(peureux,or,envie_et_devoir)
    emotion_action(curiosite_craintive,discuter)
    emotion_action(peur,fuir)
    emotion_action(agressivite,combattre)
    emotion_action(curiosite,continuer_exploration)
    emotion_action(envie,ramasser)
    emotion_action(envie_et_devoir,ramasser_apres_nettoyage_souterrain)
    sousClasseDe(envie,top_emotion)
    sousClasseDe(_8004,_8004)
    sousClasseDe(situation_non_precisee,top_situation)
    sousClasseDe(envie_et_devoir,top_emotion)
    sousClasseDe(monstre,top_situation)
    sousClasseDe(monstre,danger)
    sousClasseDe(peur,top_emotion)
    sousClasseDe(personnalite_non_precisee,top_personnalite)
    sousClasseDe(peureux,top_personnalite)
    sousClasseDe(cupide,top_personnalite)
    sousClasseDe(wumpus,monstre)
    sousClasseDe(wumpus,top_situation)
    sousClasseDe(wumpus,danger)
    sousClasseDe(objetDeValeur,top_situation)
    sousClasseDe(agressivite,top_emotion)
    sousClasseDe(curiosite,top_emotion)
    sousClasseDe(dragon,monstre)
    sousClasseDe(dragon,top_situation)
    sousClasseDe(dragon,danger)
    sousClasseDe(or,objetDeValeur)
    sousClasseDe(or,top_situation)
    sousClasseDe(danger,top_situation)
Nombre de faits vrais : 89

false.

*/