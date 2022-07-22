:- module(wellfs, [is_true/1, is_false/1, is_undefined/1, list_element/3]).

:- set_prolog_flag(toplevel_list_wfs_residual_program, false).

%%%%%%%%%% WELL FOUNDED SEMANTICS %%%%%%%%%%

% is_undefined(Atom):- Atom, tnot(Atom).
% is_true(Atom):- Atom, \+ (Atom, tnot(Atom)).
% is_false(Atom):- \+Atom.

% From Jan Wielemaker
% https://swi-prolog.discourse.group/t/unexplained-behaviour-wrt-the-well-founded-semantics-part-4/4377/2
is_undefined(Atom):- call_delays(Atom, Delays), Delays \== true.
is_true(Atom):- call_delays(Atom, true).
% is_false(Atom):- \+ is_true(Atom).
is_false(Atom):- \+ Atom.
% is_false(Atom):- call_delays(Atom, false).

list_element(Col, Row, Element):-
    is_true(cell2(Col, Row, Element)).

list_element(Col, Row, unknow):-
    is_undefined(cell2(Col, Row, unknow)).