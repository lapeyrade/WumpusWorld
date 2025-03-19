:- module(top_situation,
    [top_situation/1,
	situation_non_precisee/1,
	objetDeValeur/1,
	wumpus/1,
	monstre/1,
	danger/1,
	or/1
    ]).

:- use_module(onto).

:- discontiguous 
	top_situation/1,
	situation_non_precisee/1,
	objetDeValeur/1,
	wumpus/1,
	monstre/1,
	danger/1,
	or/1.

:- table 
	(	
		top_situation/1,
		situation_non_precisee/1,
		objetDeValeur/1,
		wumpus/1,
		monstre/1,
		danger/1,
		or/1
	)
	as 
	(incremental,dynamic)
    .	

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Ontologie des éléments de situation
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

/*---------------------------------------------------------------------------
Petite ontologie (traduite de OWL)
Attention : tous les concepts doivent au moins être sous-concept du top
concept top_situation, soit car c'est écrit explicitement
soit parce qu'on peut l'inférer
De même tous les rôles doivent au moins être rôle du top role
top_propriete (soit écrit explicitement soit inféré)
-----------------------------------------------------------------------------*/

% hiérarchie principale de concepts unaires
top_situation(X) :- danger(X).
top_situation(X) :- objetDeValeur(X).
top_situation(X) :- situation_non_precisee(X).
danger(X) :- monstre(X).
monstre(X) :- wumpus(X).
objetDeValeur(X) :- or(X).