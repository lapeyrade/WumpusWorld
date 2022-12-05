:- discontiguous 
			emotion_origine/2,
			danger/1,
			dangereux/1, 
			aAspect/2, 
			situation/2,  
			repulsion/1,
			humain/1,
			sentiment_positif/1,
			elementSituation/1,
			action_objet/2,
			objetDeValeur/1,
			objet/1,
			aValeur/2,
			obstacle/1,
			clef/1,
			mur/1,
			rocher/1,
			porte_fermee/1,
			frapper/1,
			heurter/1,
			ramasser/1.	
:- table 
	danger/1, 
	dangereux/2, 
	situation/2, 
	emotion_origine/2,
	action_objet/2,
	objet/1,
	objetDeValeur/1.

/* Hypothèse : il n'y a qu'un seul PNJ, donc tous les faits et déductions sont liés à lui.
*/

/*-----------------------------------------------
Description de départ
Récupérer du code C# dans le programme principal prolog
*/
situation(wumpus,[(x,2),(y,1)]).
situation(or,[(x,3),(y,1)]).
situation(billets,[(x,4),(y,1)]).

					/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
					Ontologie des éléments de situation
					$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

					/*---------------------------------------------------------------------------
					Règle de passage constante -> concept 
					de l'ontologie des éléments de situation 
					Question : peut-on généraliser la génération de ces règles ?
								(but : éviter de les écrire à la main et faire en sorte 
									qu'elles correspondent exactement à la héirarchie des concepts unaires
									ci-dessous)
					Remarque : les règles exprimées ici correspondent uniquement 
					aux constantes provenant du C#, et pas à tous les concepts de l'ontologie.
					-----------------------------------------------------------------------------*/
					wumpus(X):- situation(wumpus,X).
					or(X) :- situation(or,X).
					billets(X) :- situation(billets,X).
					
					/*---------------------------------------------------------------------------
					Traduction en prolog d'une petite ontologie en OWL
					-----------------------------------------------------------------------------*/

					% hiérarchie de concepts unaires
					elementSituation(X) :- etreVivant(X).	% un etre vivant est un élément de situation
					etreVivant(X) :- humain(X).				% un humain est un etre vivant
					etreVivant(X) :- monstre(X).			% un monstre est  un être vivant
					monstre(X) :- wumpus(X).				% un wumpus est un monstre
					dangereux(Y) :- agressif(Y).			% quelque chose d'agressif est dangereux

					elementSituation(X) :- danger(X).		% un danger est un élément de situation

					elementSituation(X) :- objet(X).		% un objet est un élément de situation
					% objet(X) :- objetDeValeur(X).
					
					% objetDeValeur(X) :- billets(X).

					% définition or
					% objetDeValeur(X) <-> objet(X), aValeur(X,elevee)
					objetDeValeur(X) :- objet(X), aValeur(X,elevee).
					aValeur(X,elevee) :- objetDeValeur(X).
					objet(X):-objetDeValeur(X).

					objet(X) :- or(X).
					aValeur(X,elevee) :- or(X).

					% objetDeValeur(X) :- or(X).

					% billets(X) -> objet(X),aValeur(X,faible).
					objet(X) :- billets(X).
					aValeur(X,faible) :- billets(X).



					% hiérarchie de concepts binaires
					aAspect(X,Y) :- aCaractere(X,Y). 	% si X a le caractàre Y, alors X a l'aspect Y

					% définitions de concepts pour lier entre eux

					% traduction en prolog de "r1 : danger(X) <-> elementSituation(X),aAspect(X,Y),dangereux(Y)"
					% par skolèmisation
					% i.e. X est un danger si et seulement si c'est quelque chose qui a l'aspect Y 
					% et Y une quelque chose de dangereux
					aAspect(X,f1(X)) :- danger(X).
					dangereux(f1(X)) :- danger(X).
					elementSituation(X) :- danger(X).
					danger(X) :- aAspect(X,Y),dangereux(Y).

					% traduction en prolog de "monstre(X), aCaractere(X,Y), agressif(Y) <- wumpus(X)"
					% par skolèmisation
					% i.e. si X est un wumpus, alors il a le caractère Y
					% qui est quelque chose d'agressif
					aCaractere(X,f2(X)) :- wumpus(X).
					agressif(f2(X)) :- wumpus(X).

					/*---------------------------------------------------------------------------
					Règle de passage 
					concept de l'ontologie des éléments de situation -> constante 
					-----------------------------------------------------------------------------*/
					% Alternative 1 : une règle par concept
					% situation(danger,X) :- danger(X).
					% situation(wumpus,X) :- wumpus(X).

					% Alternative 2 : le pattern query_ontology
					situation(Sit,X):-
						query_ontology(X, elementSituation, Sit).

					/* Résumé 
					Vocabulaire en entrée : wumpus, or
					Vocabulaire en sortie : wumpus, or, elementSituation, etreVivant, humain, monstre, 
											danger,	objet, objetDeValeur, billets
					*/

					%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


/*-----------------------------------------------
Description récupérée du code C# dans le programme principal prolog
*/

carac_pnj(cupide).
carac_pnj(peureux).

/*
Règles et faits du programme principal prolog 
pour générer une émotion à partir d'éléments de situation 
*/
emotion_origine(Emo,[ElemSit,X]) :- 		% l'élément de situation X génère l'émotion Emo chez le PNJ courant.
	lien_carac_emo(Car,Emo), 
	active(Car,ElemSit), 
	situation(ElemSit,X),
	carac_pnj(Car).

lien_carac_emo(peureux,peur).		% le caractère peureux est lié à l'émotion peur
lien_carac_emo(cupide,excitation).	% le caractère cupide est lié à l'émotion excitation

active(peureux,danger).			% le danger active le caractère peureux
active(cupide,objetDeValeur).	% les objets de valeur activent le caractère cupide


					/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
					Ontologie des émotions
					$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

					/*---------------------------------------------------------------------------
					Règle de passage 
					constante -> concept de l'ontologie des émotions 
					1 règle par constante
					-----------------------------------------------------------------------------*/
					peur(X) :- emotion_origine(peur,X).
					repulsion(X) :- emotion_origine(repulsion,X).
					excitation(X) :- emotion_origine(excitation,X).

					/*---------------------------------------------------------------------------
					Traduction en prolog d'une petite ontologie en OWL
					-----------------------------------------------------------------------------*/
					emotion(X) :- sentiment_negatif(X). 
					emotion(X) :- sentiment_positif(X). 
					sentiment_negatif(X) :- stress(X).
					stress(X) :- peur(X).
					sentiment_positif(X) :- plaisir(X).
					plaisir(X) :- excitation(X).

					/*---------------------------------------------------------------------------
					Règle de passage 
					concept de l'ontologie des émotions -> constante 
					-----------------------------------------------------------------------------*/
					% Alternative 1 : une règle par concept
					% emotion_origine(peur,X) :- peur(X).
					% emotion_origine(repulsion,X) :- repulsion(X).

					% Alternative 2 : le pattern query_ontology
					emotion_origine(Emotion,X):-
						query_ontology(X, emotion, Emotion).

					/* Résumé 
					Vocabulaire en entrée : peur, répulsion, excitation
					Vocabulaire en sortie : peur, répulsion, excitation, emotion, sentiment_negatif, 
											sentiment_positif, stress, plaisir
					*/
					%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$


/*
Règles et faits du programme principal prolog 
pour générer un jugement à partir des émotions 
*/

jugement(LDesir,LRepulsion) :- 
	findall(repulsion(X),emotion_origine(sentiment_negatif,X),LRepulsion),
	findall(desir(X),emotion_origine(sentiment_positif,X),LDesir).

/*
Règles et faits du programme principal prolog 
pour générer la prochaine action à partir des émotions
*/

action_envisageable(Action,[Y2,X]) :-	%	Action est une action envisageable à faire dans la cellule X si
	jugement(LDesir,_),					%	- quand on regarde la liste des cases désirables,
	member(desir([Y,X]),LDesir),		%		on trouve la case X associée à l'objet Y qui la rend désirable,
	action_objet(Action,Y2),			%	- et Action a pour objet Y.
	query_ontology(X, Y, Y2).			

action_objet(ramasser,or).


					/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
					Ontologie des actions
					$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

					/*---------------------------------------------------------------------------
					Règle de passage 
					constante -> concept de l'ontologie des actions 
					1 règle par constante
					-----------------------------------------------------------------------------*/
					ramasser(X) :- action_objet(ramasser,X).

					/*---------------------------------------------------------------------------
					Attention ici on modélise un peu différemment de précédemment:
					action(Y) ne signifie pas Y est une action, mais 
							action a comme objet Y (autrement dit action s'applique à Y)
					==> l'ontologie n'est pas issu d'une ontologie OWL, c'est une 
							ontologie ad-hoc à nos besoins ici.
					Possibilité d'évolution : modéliser cette ontologie comme précédemment.
					-----------------------------------------------------------------------------*/
					action(Y) :- heurter(Y).
					action(Y) :- ramasser(Y).
					action(Y) :- frapper(Y).

					% heurter(Y) :- heurter_obstacle(Y).
					% heurter_obstacle(Y) :- obstacle(Y).

					% ramasser(Y) :- objet(Y).

					% object(Y):- obstacle(Y).
					% object(Y):- objetDeValeur(Y).
					% objetDeValeur(Y) :- clef(Y).

					% obstacle(Y):- mur(Y).
					% obstacle(Y):- rocher(Y).
					% obstacle(Y):- porte_fermee(Y).

					/*---------------------------------------------------------------------------
					Règle de passage 
					concept de l'ontologie des actions -> constante 
					-----------------------------------------------------------------------------*/
					% Alternative 2 : le pattern query_ontology
					action_objet(Action,Y):-
						query_ontology(Y, action, Action).
					%$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$





%----------------------------------------------
%----------------------------------------------
%----------------------------------------------
/* 
*/




/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Pattern query_ontology
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

% situation(wumpus,[(x,2),(y,1)]).
% query_ontology([(x,2),(y,1)],wumpus,wumpus) renvoie
%		vrai si dans l'ontologie en prolog on a
%			- le fait wumpus([(x,2),(y,1)]).
%			- ou la règle wumpus([(x,2),(y,1)]) :- corps-de-regle. et call(corps-de-regle) renvoie vrai.
%			- ou ...

query_ontology(X, Atom, Atom):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	call(A).

query_ontology(X, Atom, Atom2):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	% comma_list(A,LA),
	% length(LA,1),
	call(A),
	A =.. [Atom3, _],
	query_ontology(X, Atom3, Atom2).




/*

%----------------------------------------------
% Récupérer un foncteur 
situationBis(W,X,Y) :- 
	danger(T),
	T=..[W|[X,Y]]
	.
danger(w(1,2,3)).

*/

 /* Predicates to trace the execution
*/

:- initialization(globVar).
globVar :- 
	nb_setval(tab,"").

traceAll(Atom,Res) :-
	term_variables(Atom, L),
	findall(L,callTrace(Atom),Res),
	displayList(Res),nl.


callTrace(Atom) :- 
	sleep(0.05),
	b_getval(tab,OldIndent),
	concat(OldIndent,"|    ",NewIndent),
	b_setval(tab,NewIndent),

	write(NewIndent),
	term_string(Atom,StringAtom),
	write(StringAtom),
	write(" ???"),
	flush_output,
	nl,

	(Atom *->	
		(
		((b_getval(tab,Indent2), Indent2 == NewIndent)
		  *-> true
		  ; b_setval(tab,NewIndent)
		),
		write(NewIndent),
		write("--> "), 
		term_string(Atom,StringAtom2),
		writeln(StringAtom2),
		flush_output
		)
	;
		(
		write(NewIndent),
		write("--> FAILURE/SUSPENDED : "),
		%term_string(Atom,StringAtom3),
		%write(StringAtom3),
		write(Atom),
		writeln(" is false/suspended"),
		flush_output,
		false
		)		
	)
	. 
	
displayList([]).
displayList([H|T]):-
	writeln(H),
	displayList(T).
 


% ?- emotion(X, Emotion).
% X = [human0, greedy, common_object],
% Emotion = emotion ;
% X = [human0, greedy, common_object],
% Emotion = anger ;
% X = [human0, greedy, common_object],
% Emotion = good_emotion ;
% X = [human0, greedy, common_object],
% Emotion = bad_emotion ;
% X = [human0, greedy, common_object],
% Emotion = joy ;
% X = [human0, greedy, unvaluable_object],
% Emotion = anger ;
% X = [human0, greedy, unvaluable_object],
% Emotion = emotion ;
% X = [human0, greedy, unvaluable_object],
% Emotion = bad_emotion ;
% X = [human0, greedy, valuable_object],
% Emotion = emotion ;
% X = [human0, greedy, valuable_object],
% Emotion = good_emotion ;
% X = [human0, greedy, valuable_object],
% Emotion = joy.