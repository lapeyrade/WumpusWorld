:- module(top_personnalite,
    [
		top_personnalite/1,
		personnalite_non_precisee/1,
		peureux/1,
		cupide/1
    ]).

:- use_module(onto).

:- discontiguous 
		top_personnalite/1,
		personnalite_non_precisee/1,
		peureux/1,
		cupide/1.

:- table 
	(	
		top_personnalite/1,
		personnalite_non_precisee/1,
		peureux/1,
		cupide/1
	)
	as 
	(incremental,dynamic)
    .	

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Ontologie des traits de caractère
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

top_personnalite(X) :- peureux(X).
top_personnalite(X) :- cupide(X).
top_personnalite(X) :- personnalite_non_precisee(X).