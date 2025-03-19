:- module(onto,
    [classification/2,
	subsumedBy/2,
	strictSubsumedBy/2,
	is_instance_of/2,
	triple/3,
	data_concept/2
    ]).

:- discontiguous
	subsume/2,
	classification/2,
	classification2/2,
	subsumedBy/2,
	strictSubsumedBy/2,
	is_instance_of/2,
	triple/3,
	data_concept/2
	.

:- table (
	subsume/2,
	subsume_intersection/2,
	classification/2,
	classification2/2, 
	is_instance_of/2,
	triple/3,
	data_concept/2
	% subsumedBy/2, % Do not table this predicate!
    ) 
    as (incremental,dynamic).

:- dynamic subsumedBy/2,strictSubsumedBy/2.

%***************************************************************************************************************************
%***************************************************************************************************************************
%***************************************************************************************************************************
% CLASSIFICATION
/*
This file provides all predicates that can be used to classify an OWL ontology
that can be translated via skolemization into a set of (nearly) equivalent prolog 
rules containing not negated atoms. So all sublanguages of OWL cannot be classified here,
only the least expressive ones.
For example the EL description logic with role subsumption (grounding the OWL 3 EL profile)
can be translated so that classification can be achieved. 
*/

/*---------------------------------------------------------------------------------
classification(TopConcept,Arity)

Aims at generating all possible assertz(subsumedBy(C1,C2)) such that the top concept
of all these subsumption relations is TopConcept having arity Arity.

classification/2 calls subsumes/3 and classification2/2.

- first it calls subsumes to find all subconcepts of TopConcept.
- then it calls classification2 that will itself call classification 
with all subconcepts of TopConcept (so that the full subsumtion hierarchy is inferred).
- then it assert the (trivial) reflexive subsumption relationship for TopConcept.
------------------------------------------------------------------------------------*/
classification(TopConcept,Arity):-
	findall([SubConcept,TopConcept],subsumes(TopConcept,Arity,SubConcept),ListClassif),
	classification2(ListClassif,Arity),
	assertz(subsumedBy(TopConcept,TopConcept)).

/*---------------------------------------------------------------------------------
classification2([FirstCouple|OtherCouples],Arity)

For each couple of concepts (C1,C2), from the list [FirstCouple|OtherCouples] of 
couples, where it has been showned that subsumes(C2,Arity,C1) is true  (i.e. C1 is 
subsumed by C2), this predicates determines all other concept C3 such that 
subsumes(C1,Arity,C3) is true (i.e. C3 is subsumed by C1).

classification2/2 calls itself and classification/2.

- first check if the subsumption relation between C1 and C2 (i.e. C1 subsumed by C2)
is not either the trivial (reflexive) one, or has not been already inferred (it is then
a fact of the subsumedBy/2 predicate) : in that case, it forgets (C1,C2) and 
calls itself on the list queue OtherCouples.
- if it is not the case, it creates the fact subsumedBy(C1,C2) and asserts it, and then 
it calls classification on C1 (to infer all subconcepts of C1), and then calls itself 
on the list queue OtherCouples.
------------------------------------------------------------------------------------*/
classification2([],_).
classification2([FirstCouple|OtherCouples],Arity):-
	FirstCouple = [C1,C2],
	(((C1==C2);(subsumedBy(C1,C2)))
	->
		classification2(OtherCouples,Arity)
	;
		Atom =.. [subsumedBy|FirstCouple],
		assertz(Atom),
		classification(C1,Arity),
		classification2(OtherCouples,Arity)
	).

%***************************************************************************************************************************
%***************************************************************************************************************************
%***************************************************************************************************************************
% SUBSUMPTION
/*
there are 4 subsumption predicates :
- subsumes/3 : internal, used by classification/2
- subsume/2 : internal, used by subsumes/3 and itself
- subsumedBy/2 : external (for users of this file), built by assertz/1 calls 
				in classification/2
- strictSubsumedBy/2
*/

/*---------------------------------------------------------------------------------
subsumes(Concept1,Arity1,Concept2)

Find all couples of predicates Concept1 and Concept2
such that Concept 1 subsumes Concept 2.
Concept1 and Concept2 are concept names (not an atom).
Arity1 is the arity of Concept 1 and is at least 1 (i.e. this predicate
does not infer subsumption relationships between O-ary predicates).
Concept 1 must be instanciated before execution.
Arity 1 should be instanciated before execution even if it is not mandatory in some cases.

subsumes/3 calls subsume/2.

- it first builds an atom Atom1 having Concept1 as predicate and generate as many new 
variables as Arity1
- then it calls subsume/2 to get all atoms Atom2 such that the rule
	Atom1 :- Atom2. 
	(with Atom1 and Atom2 being atoms with the same variables as arguments)
is true.
then it deconstructs Atom2 to keep its predicate as Concept2 and checks
if arguments of Atom2 are syntactically the same as those of Atom1.
Indeed, subsume(Atom1,Atom2) may be true for example for Atom1 = p(X) and Atom2 = r(toto),
meaning that the rule 
	p(X) :- r(toto).
is true.
	However, it does not mean that r is subsumed by p 
(whereas when the rule p(X) :- r(X) is true, it means that r is subsumed by p). 
So a final check must be achieved.
------------------------------------------------------------------------------------*/

subsumes(Concept1,Arity1,Concept2):-
	length(L1,Arity1),
	Atom1 =.. [Concept1|L1],
	subsume(Atom1,Atom2),
	Atom2 =.. [Concept2|L2],
	L1==L2.

/*---------------------------------------------------------------------------------
subsume(HeadAtom,SubsumedAtom)

Find all couples (HeadAtom,SubsumedAtom) of atoms (an atom is a term like danger(X), or 
hasChild(X,Y)), such that the rule 
HeadAtom :- SubsumedAtom.
(with HeadAtom and SubsumedAtom being atoms with the same variables as arguments)
is true.
Remark (just to be sure) : SubsumedAtom is an atom, not a conjunction of atoms.

subsume/2 calls subsume_intersection/2. 

- first it calls the meta predicate clause to find all rules which head atom unifies 
with HeadAtom (so subsume/2 is a kind of meta interpreter).
- if there is such a rule, it tests if it is a fact. If it is a fact, it is not 
useful for our purpose, so the predicate fails.
- if it is not a fact but a rule with at least one atom in the body, then the body
of this rule is first transformed into the prolog list AtomBodyList of atoms thanks to the 
listp_to_list/2 predicate, then is called the subsume_intersection/2 which finds all
atom (the SubsumedAtom argument) that are subsumed by all atoms in AtomBodyList.
------------------------------------------------------------------------------------*/
subsume(HeadAtom,HeadAtom).

subsume(HeadAtom,SubsumedAtom):-			
	clause(HeadAtom,AtomBody),					
	(AtomBody \= true
	->
			listp_to_list(AtomBody,AtomBodyList),
			subsume_intersection(AtomBodyList,SubsumedAtom)
	).

% Old version
% subsume(HeadAtom,SubsumedAtom):-
% 	copy_term(HeadAtom,HeadAtom2),				
% 	clause(HeadAtom2,AtomBody),					
% 	(AtomBody \= true
% 	->
% 			HeadAtom=HeadAtom2,
% 			listp_to_list(AtomBody,AtomBodyList),
% 			subsume_intersection(AtomBodyList,SubsumedAtom)
% 	).
	

/*---------------------------------------------------------------------------------
subsume_intersection(HeadAtomList,SubsumedAtom)

Finds all atom (in the SubsumedAtom argument) that are subsumed by all atoms in HeadAtomList.

subsume_intersection/2 calls subsume/2 and subsume_intersection/2. 

- if there is one single atom in HeadAtomList, it calls subsume/2 on this atom.
- else  
	- it checks if there are at least 2 atoms, 
	- it (indirectly recursively, via subsume/2) finds all atoms SubsumedAtom that are subsumed 
	by the first one in HeadAtomList
	- it (directly recursively, via itself) checks whether each remaining atom in HeadAtomList subsumes
	SubsumedAtom.
------------------------------------------------------------------------------------*/
 subsume_intersection([HeadAtom],SubsumedAtom):-
	subsume(HeadAtom,SubsumedAtom).
 
 subsume_intersection(HeadAtomList,SubsumedAtom):-
	 length(HeadAtomList,N),
	 N>1,
	 HeadAtomList = [HeadAtom|HeadAtomListRest],
	 subsume(HeadAtom,SubsumedAtom),
	 subsume_intersection(HeadAtomListRest,SubsumedAtom).
 

/*---------------------------------------------------------------------------------
strictSubsumedBy(X,Y)

Gives all couples (X,Y) of concept names such that X is strictly subsumed by Y.

subsume_intersection/2 calls subsumedBy/2. 
------------------------------------------------------------------------------------*/
strictSubsumedBy(X,Y) :- 
	subsumedBy(X,Y), 
	X\=Y.



%***************************************************************************************************************************
%***************************************************************************************************************************
%***************************************************************************************************************************
% USEFUL PREDICATES

/*---------------------------------------------------------------------------------
no_list_as_argument(X)

IF X is not a liste but a term like p(arg1,...argn), then no_list_as_argument(X)
checks whether no argument in its argument list [arg1,...argn] is itself a list.
If X is a list, then no_list_as_argument(X) checks whether none of its elements is 
itself a list.
Remark : beware of multiple level nesting because checking stops at the first 
level of nesting.
Example: 
no_list_as_argument(p(f([e,f]))) returns true.

no_list_as_argument/1 is recursive.
------------------------------------------------------------------------------------*/
no_list_as_argument(Atom):-
	\+ is_list(Atom),
	Atom =.. [_|ArgList],
	no_list_as_argument(ArgList).

no_list_as_argument([]).
no_list_as_argument([Arg|ArgList]):-
	\+ is_list(Arg),
	no_list_as_argument(ArgList).


/*---------------------------------------------------------------------------------
listp_to_list(Listp,List)

Transforms a list with parenthesis into a list with square brackets.
Ex. (a,b,c,d) becomes [a,b,c,d]
------------------------------------------------------------------------------------*/

listp_to_list(Listp,List):-
    Listp =.. [Op|_],
    (Op \= ','
    ->
        List=[Listp]
    ;
        Listp =.. [',',X|[TListp]],
        listp_to_list(TListp,SubList),
        append([X],SubList,List)
    ).





%***************************************************************************************************************************
%******* Interface entre un code prolog principal et des ontologies
%***************************************************************************************************************************

/*$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
Prédicats appelés par le code C#
is_instance_of(Instance,Concept) : pour faire un assert du fait Concept(Instance)
triple(Instance1,Predicat,Instance2) : pour faire un assert du fait Predicat(Instance1,Instance2)
où
	- Concept doit être un nom de concept d'une ontologie référencée 
		par le code prolog principal
	- Predicat est un prédicat du code prolog principal
	- Instance est de la forme [id_instance|[...]]
		Exemple : [conan,[(2,4),(3,1)]]

Les rôles des ontologies restent internes aux ontologies. En effet, les concepts
des ontologies doivent être vus comme des types de données avancés avec lesquels
on peut raisonner grâce à leur ontologie d'appartenance. Les rôles de ces mêmes ontologies
ne peuvent donc être vus aussi comme des types. Ils restent donc restreint aux raisonnements
sur l'ontologie et ne peuvent pas être utilisés par le code C#.
$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$*/

% Où Concept est un concept d'une ontologie
% Instance et Concept doivent être instanciés à l'appel
% ATTENTION ICI : besoin d'ajouter en dur user: à l'atome 
% dans le assertz : je n'ai pas trouver comment récupérer le module du code
% qui exécute l'appel à is_instance_of() pour que l'approche marche dans tout code.
% AUTREMENT DIT : ce prédicat marche si appelé dans le module user.
is_instance_of(Instance,Concept):-
	Atom =.. [Concept,Instance],
	assertz(user:Atom),
	assertz(data_concept(Instance,Concept)).

% Où Predicat est un prédicat du code prolog principal hors ontologie
% Instance1, Instance2 et Predicat doivent être instanciés à l'appel.
% Les concepts-types de Instance1 et Instance2 sont inférés à partir 
% de la définition du prédicat qui précise les concepts-types pour chaque argument.
% ATTENTION ICI : besoin d'ajouter en dur user: à l'atome 
% dans le assertz : je n'ai pas trouver comment récupérer le module du code
% qui exécute l'appel à is_instance_of() pour que l'approche marche dans tout code.
% AUTREMENT DIT : ce prédicat marche si appelé dans le module user.
triple(Instance1,Predicat,Instance2):-
	Atom =.. [Predicat,Instance1,Instance2],
	assertz(user:Atom).

% Récupère ou teste les couples (Instance,Concept) tels
% que Concept(Instance) est vrai.
% Instance et Concept peuvent ne pas être instanciés à l'appel.
data_concept(Instance,Concept):-
	data_concept(Instance,SubConcept),
	strictSubsumedBy(SubConcept,Concept).





	