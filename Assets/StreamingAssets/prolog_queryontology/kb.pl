% kb.pl
:- consult([situation, action]).

%-----------------------------------
/* The query_ontology predicate */
query_ontology(X, Atom, Atom):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	call(A).

query_ontology(X, Atom, Atom2):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	call(A),
	A =.. [Atom3, _],
	query_ontology(X, Atom3, Atom2).
%-----------------------------------

/* Facts */
situation(agent0, [0,0]).
situation(agent1, [0,0]).
situation(agent2, [0,0]).
situation(gold0, [0,0]).
situation(gold3, [2,2]).
situation(iron, [3,3]).
situation(iron2, [4,4]).
situation(rock, [5,5]).
situation(wall, [0,1]).
situation(wall, [0,2]).
situation(wumpus, [0, 10]).
situation(pit, [11, 11]).
situation(breeze, [11, 12]).
situation(stench, [10, 11]).
situation(sword, [1, 1]).
situation(bow, [1, 1]).
situation(arrow, [1, 1]).

trait(agent0, cupid).
trait(agent1, reckless).
trait(agent2, fearful).