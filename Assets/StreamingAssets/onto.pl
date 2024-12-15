:- table (subsume/2, classification/2, classification2/2, is_instance_of/2, 
    triple/3, data_concept/2) as (incremental,dynamic).
:- dynamic subsumedBy/2, strictSubsumedBy/2.


% Aims at generating all possible assertz(subsumedBy(C1,C2)) such that the top concept
% of all these subsumption relations is TopConcept having arity Arity.
classification(TopConcept,Arity):-
	findall([SubConcept,TopConcept],subsumes(TopConcept,Arity,SubConcept),ListClassif),
	classification2(ListClassif,Arity),
	assertz(subsumedBy(TopConcept,TopConcept)).

classification2([],_).
classification2([FirstCouple|OtherCouples],Arity):-
	FirstCouple = [C1,C2],
	(((C1==C2);(subsumedBy(C1,C2)))
	->
		classification2(OtherCouples,Arity)
	;
		Atom =.. [subsumedBy|FirstCouple],
		assertz(Atom),
        % writeln(assertz(Atom)),
		classification(C1,Arity),
		classification2(OtherCouples,Arity)
	).

/*---------------------------------------------------------------------------------
subsumes/3
Find all couples of predicates Concept1 and Concept2
such that Concept 1 subsumes Concept 2
Arity1 is the arity of Concept 1
Concept 1 must be instanciated before execution.
Arity 1 should be instanciated before execution but it is not mandatory.
------------------------------------------------------------------------------------*/

subsumes(Concept1,Arity1,Concept2):-
	length(L1,Arity1),
	Atom1 =.. [Concept1|L1],
	subsume([Atom1],Atom2),
	Atom2 =.. [Concept2|L2],
	L1==L2.


subsume([AtomHead],AtomHead).

subsume([AtomHead],SubsumedAtom):-
	copy_term(AtomHead,AtomHead2),
	clause(AtomHead2,AtomBody),
	(AtomBody=true
	->
		AtomHead=AtomHead2,
		SubsumedAtom = AtomHead
	;
		(no_list_as_argument(AtomHead2)
		->
			AtomHead=AtomHead2,
			listp_to_list(AtomBody,AtomBodyList),
			subsume_intersection(AtomBodyList,SubsumedAtom)
		;
			SubsumedAtom = AtomHead
		)
	).
 
 subsume_intersection([AtomHead],SubsumedAtom):-
	 subsume([AtomHead],SubsumedAtom).
 
 subsume_intersection(AtomHeadList,SubsumedAtom):-
	 length(AtomHeadList,N),
	 ground(N),
	 N>1,
	 AtomHeadList = [AtomHead|AtomHeadListRest],
	 subsume([AtomHead],SubsumedAtom),
	 subsume_intersection(AtomHeadListRest,SubsumedAtom).
 
strictSubsumedBy(X,Y) :- 
	subsumedBy(X,Y), 
	X\=Y.

 
% Prédicats utilitaires pour subsume/2
no_list_as_argument(Atom):-
	\+ is_list(Atom),
	Atom =.. [_|ArgList],
	no_list_as_argument(ArgList).

no_list_as_argument([]).
no_list_as_argument([Arg|ArgList]):-
	\+ is_list(Arg),
	no_list_as_argument(ArgList).

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
