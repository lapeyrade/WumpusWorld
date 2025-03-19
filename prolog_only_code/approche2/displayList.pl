:- module(displayList,
    [
	displayList/1,
	displayList/2,
	displayListToString/3
    ]).

displayList([]).
displayList([H|T]):-
	writeln(H),
	displayList(T).
	
displayList([],_).
displayList([H|T],Indent):-
	write(Indent),
	writeln(H),
	displayList(T,Indent).

displayListToString([],_,"").
displayListToString([H|T],Indent,StringRes):-
	term_string(H,StringH),
	string_concat(Indent,StringH,Res1),
	((T=[])*->
		StringRes1 = Res1
	;
		string_concat(Res1,"\n",StringRes1)
	),
	displayListToString(T,Indent,StringRes2),
	string_concat(StringRes1,StringRes2,StringRes).
