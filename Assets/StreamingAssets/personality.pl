:- table (ambitious/1, disciplined/1, sensitive/1, cupid/1, 
    brave/1, ascetic/1, coward/1) as (incremental, dynamic).

/*** Personalities Hierachy ***/
personality(X) :- ambitious(X).
personality(X) :- disciplined(X).
personality(X) :- sensitive(X).
ambitious(X) :- cupid(X).
ambitious(X) :- brave(X).
disciplined(X) :- ascetic(X).
sensitive(X) :- coward(X).