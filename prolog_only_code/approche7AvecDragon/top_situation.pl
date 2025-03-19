:- module(top_situation,
    [		top_situation/1,
    situation_non_precisee/1,
    objetDeValeur/1,
    wumpus/1,
    monstre/1,
    danger/1,
    or/1,
    dangereux/1, 
    monstrueux/1,
    oeilUnique/1,
    agressif/1,
    aAspect/2,
    top_propriete/2, 
    aCaractere/2,
    aTraitPhysique/2,
    dragon/1,
    cracheDuFeu/1
    ]).

:- use_module(onto).

:- discontiguous 
top_situation/1,
situation_non_precisee/1,
objetDeValeur/1,
wumpus/1,
monstre/1,
danger/1,
or/1,
dangereux/1, 
monstrueux/1,
oeilUnique/1,
agressif/1,
aAspect/2,
top_propriete/2, 
aCaractere/2,
aTraitPhysique/2,
dragon/1,
cracheDuFeu/1.

:- table 
	(	
		top_situation/1,
		situation_non_precisee/1,
		objetDeValeur/1,
		wumpus/1,
		monstre/1,
		danger/1,
		or/1,
		dangereux/1, 
        monstrueux/1,
        oeilUnique/1,
		agressif/1,
		aAspect/2,
		top_propriete/2, 
		aCaractere/2,
        aTraitPhysique/2,
        dragon/1,
        cracheDuFeu/1
	)
	as 
	(incremental,dynamic)
    .	

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Ontologie des éléments de situation - Approche 7
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*---------------------------------------------------------------------------
Petite ontologie (traduite de OWL)
Attention : tous les concepts doivent au moins être sous-concept du top
concept top_situation, soit car c'est écrit explicitement
soit parce qu'on peut l'inférer
De même tous les rôles doivent au moins être rôle du top role
top_propriete (soit écrit explicitement soit inféré)
-----------------------------------------------------------------------------*/

% Hiérarchie de concepts
top_situation(X) :- danger(X).
top_situation(X) :- objetDeValeur(X).
top_situation(X) :- situation_non_precisee(X).
top_situation(X) :- monstre(X).					
% monstre(X) :- wumpus(X).                     
objetDeValeur(X) :- or(X).
dangereux(X) :- agressif(X).					
monstrueux(X) :- oeilUnique(X).                

% Cas du dragon
monstrueux(X) :- cracheDuFeu(X).                



% traduction de "danger \equiv \exists aAspect.dangereux"
% i.e. de "danger(X) <-> aAspect(X,Y),dangereux(Y)"
% par skolémisation
aAspect(X,f1(X)) :- danger(X).					
dangereux(f1(X)) :- danger(X).					
danger(X) :- aAspect(X,Y),dangereux(Y).			

% traduction de "monstre \sqsubseteq \exists aCaractere.agressif"
% i.e. "aCaractere(X,Y), agressif(Y) <- monstre(X)"
% par skolémisation
aCaractere(X,f2(X)) :- monstre(X).			
agressif(f2(X)) :- monstre(X).				

% Hiérarchie de rôles
top_propriete(X,Y) :- aAspect(X,Y).				
% top_propriete(X,Y) :- aCaractere(X,Y).			
aAspect(X,Y) :- aCaractere(X,Y).				
aAspect(X,Y) :- aTraitPhysique(X,Y).		   	

% traduction de "monstre \equiv \exists aAspect.monstrueux"
% i.e. "aASpect(X,Y), monstrueux(Y) <-> monstre(X)"
% par skolémisation
monstre(X) :- aAspect(X,Y),monstrueux(Y).       
aAspect(X,f3(X)) :- monstre(X).                 
monstrueux(f3(X)) :- monstre(X).               

% traduction de "wumpus \sqsubseteq \exists aTraitPhysique.oeilUnique"
% i.e. "aTraitPhysique(X,Y), oeilUnique(Y) <- wumpus(X)"
% par skolémisation
aTraitPhysique(X,f4(X)) :- wumpus(X).           
oeilUnique(f4(X)) :- wumpus(X).                 

% Cas du dragon
% traduction de "dragon \sqsubseteq \exists aTraitPhysique.cracheDuFeu"
% i.e. "aTraitPhysique(X,Y), cracheDuFeu(Y) <- dragon(X)"
% par skolémisation
aTraitPhysique(X,f5(X)) :- dragon(X).           
cracheDuFeu(f5(X)) :- dragon(X).  