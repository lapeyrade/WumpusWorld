%% Ontology System - Core ontological reasoning engine
%% This file implements the fundamental ontology operations and concept relationships

%% Dynamic and incremental tabling declarations for ontology predicates
%% Ensures efficient querying and updates of the ontological knowledge base
:- table (subsume/2, classification/2, classification2/2, subsumes/3, is_instance_of/2,
	subsume_intersection/2, triple/3, data_concept/2, no_list_as_argument/1, listp_to_list/2,
	listp_to_list_acc/3) as (incremental,dynamic).

%% Dynamic declarations for subsumption relationships
:- dynamic subsumedBy/2, strictSubsumedBy/2.

%% Classification System
%% Generates all possible subsumption relationships for a given concept hierarchy

%% Main Classification Predicate
%% Generates and asserts all subsumption relationships for a top concept
%% TopConcept: The root concept of the hierarchy
%% Arity: Number of arguments for the concept predicate
classification(TopConcept,Arity):-
	findall([SubConcept,TopConcept],subsumes(TopConcept,Arity,SubConcept),ListClassif),
	classification2(ListClassif,Arity),
	assertz(subsumedBy(TopConcept,TopConcept)).

%% Helper Classification Predicate
%% Processes the list of subsumption relationships
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

%% Subsumption System
%% Core predicates for determining concept relationships

%% Subsumes Predicate
%% Finds all pairs of concepts where one subsumes the other
%% Concept1: The potentially subsuming concept
%% Arity1: Arity of Concept1
%% Concept2: The potentially subsumed concept
subsumes(Concept1,Arity1,Concept2):-
	length(L1,Arity1),
	Atom1 =.. [Concept1|L1],
	subsume([Atom1],Atom2),
	Atom2 =.. [Concept2|L2],
	L1==L2.

%% Direct Subsumption Check
subsume([AtomHead],AtomHead).

%% Indirect Subsumption Check
%% Handles complex subsumption cases with body conditions
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

%% Intersection Subsumption
%% Handles subsumption through multiple conditions
subsume_intersection([AtomHead], SubsumedAtom):-
	subsume([AtomHead], SubsumedAtom).
subsume_intersection([AtomHead|Rest], SubsumedAtom):-
	Rest \= [],
	subsume([AtomHead], SubsumedAtom),
	subsume_intersection(Rest, SubsumedAtom).

%% Strict Subsumption
%% Defines strict subsumption (proper subset relationship)
strictSubsumedBy(X,Y):-
	subsumedBy(X,Y),
	X\=Y.

%% Utility Predicates
%% Helper predicates for handling complex terms and lists

%% List Argument Check
%% Ensures no list arguments in atoms
no_list_as_argument(Atom):-
	\+ is_list(Atom),
	Atom =.. [_|ArgList],
	no_list_as_argument(ArgList).

no_list_as_argument([]).
no_list_as_argument([Arg|ArgList]):-
	\+ is_list(Arg),
	no_list_as_argument(ArgList).

%% List Processing
%% Converts between list representations
listp_to_list(Listp,List):-
    listp_to_list_acc(Listp, [], List).

listp_to_list_acc(Listp, Acc, List):-
    Listp =.. [Op|Args],
    (Op \= ',' ->
        reverse([Listp|Acc], List)
    ;
        Args = [X,TListp],
        listp_to_list_acc(TListp, [X|Acc], List)
    ).

%% C# Interface System
%% Predicates for interfacing with C# code

%% Instance Assertion
%% Asserts that an instance belongs to a concept
%% Instance: [id_instance|[...]] format
%% Concept: Target concept from ontology
is_instance_of(Instance,Concept):-
	Atom =.. [Concept,Instance],
	assertz(user:Atom),
	assertz(data_concept(Instance,Concept)).

%% Triple Assertion
%% Asserts a relationship between two instances
%% Instance1, Instance2: The related instances
%% Predicat: The relationship predicate
triple(Instance1,Predicat,Instance2):-
	Atom =.. [Predicat,Instance1,Instance2],
	assertz(user:Atom).

%% Tests if Instance belongs to Concept or any
%% of its subconcepts through transitive subsumption
data_concept(Instance,Concept):-
	data_concept(Instance,SubConcept),
	strictSubsumedBy(SubConcept,Concept).