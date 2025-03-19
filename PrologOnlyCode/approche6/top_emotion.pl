:- module(top_emotion,
	[top_emotion/1,
	peur/1,
	envie/1,
	curiosite/1,
	desir/1,
	envie_et_devoir/1,
	agressivite/1]).

:- use_module(onto).

:- discontiguous 
	top_emotion/1,
	peur/1,
	envie/1,
	curiosite/1,
	desir/1,
	envie_et_devoir/1,
	agressivite/1.

:- table 
	(	
		top_emotion/1,
		peur/1,
		envie/1,
		curiosite/1,
		desir/1,
		envie_et_devoir/1,
		agressivite/1
	)
	as 
	(incremental,dynamic)
    .	

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Ontologie des émotions
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

top_emotion(X) :- envie(X).
top_emotion(X) :- peur(X).
top_emotion(X) :- agressivite(X).
top_emotion(X) :- curiosite(X).
envie(X) :- envie_et_devoir(X).
envie(X) :- desir(X).