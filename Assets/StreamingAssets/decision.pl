query_onto(X, Atom, Atom):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	call(A).

query_onto(X, Atom, Atom2):-
	PredicateAtom =.. [Atom, X],
	clause(PredicateAtom, A),
	call(A),
	A =.. [Atom3, _],
	query_onto(X, Atom3, Atom2).

%%% QUERY ELEMENT %%%
element(Id, Element):-
    query_onto(Id, element, Element).

%%% QUERY EMOTION %%%
emotion(Id, Emotion):-
    query_onto(Id, emotion, Emotion).

emotion([Id, Personality, Element], Emotion):-
    personality(Id, Personality),
	element(_, Element),
    perso_produce_emo(Personality, Emotion),
	elem_activate_perso(Element, Personality).
    % elem_activate_emo(Element, Emotion).

%%% QUERY ACTION %%%
% action(Id, Action):-
% 	query_onto(Id, action, Action).

%%% FACTS %%%
situation(human0, [0,0]).
situation(gold0, [1,1]).
situation(gold3, [2,2]).
situation(iron, [3,3]).
situation(iron2, [4,4]).
situation(rock, [5,5]).
situation(wall, [0,1]).
situation(wall, [0,2]).
situation(wumpus, [10, 10]).
situation(pit, [11, 11]).
situation(breeze, [11, 12]).
situation(stench, [10, 11]).

% personality(human0, coward).
% personality(human0, impatient).
% personality(human0, fragile).
% personality(human0, naive).
personality(human0, greedy).

perso_produce_emo(coward, fear).
perso_produce_emo(coward, surprise).
perso_produce_emo(coward, joy).
perso_produce_emo(impatient, anger).
perso_produce_emo(impatient, joy).
perso_produce_emo(fragile, joy).
perso_produce_emo(fragile, disgust).
perso_produce_emo(fragile, surprise).
perso_produce_emo(naive, surprise).
perso_produce_emo(naive, joy).
perso_produce_emo(greedy, joy).
perso_produce_emo(greedy, anger).

elem_activate_perso(monster, coward).
elem_activate_perso(breeze, coward).
elem_activate_perso(stench, coward).
elem_activate_perso(obstacle, impatient).
elem_activate_perso(unvaluable_object, impatient).
elem_activate_perso(trap, impatient).
elem_activate_perso(monster, fragile).
elem_activate_perso(obstacle, fragile).
elem_activate_perso(stench, fragile).
elem_activate_perso(monster, naive).
elem_activate_perso(trap, naive).
elem_activate_perso(unvaluable_object, greedy).
elem_activate_perso(valuable_object, greedy).
elem_activate_perso(common_object, greedy).

% elem_activate_emo(monster, fear).
% elem_activate_emo(monster, surprise).
% elem_activate_emo(monster, joy).
% elem_activate_emo(monster, disgust).
% elem_activate_emo(breeze, fear).
% elem_activate_emo(breeze, surprise).
% elem_activate_emo(stench, fear).
% elem_activate_emo(stench, surprise).
% elem_activate_emo(obstacle, anger).
% elem_activate_emo(trap, surprise).
% elem_activate_emo(trap, fear).
% elem_activate_emo(unvaluable_object, anger).
% elem_activate_emo(unvaluable_object, sad).
% elem_activate_emo(valuable_object, joy).
% elem_activate_emo(valuable_object, surprise).
% elem_activate_emo(common_object, joy).
% elem_activate_emo(common_object, anger).

% ?- emotion(X, Emotion).
% X = [human0, greedy, unvaluable_object],
% Emotion = joy ;
% X = [human0, greedy, unvaluable_object],
% Emotion = good_emotion ;

% X = [human0, greedy, valuable_object],
% Emotion = anger ;
% X = [human0, greedy, valuable_object],
% Emotion = bad_emotion ;