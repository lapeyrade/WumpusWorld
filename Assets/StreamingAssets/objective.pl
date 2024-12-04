:- table(objective/1, success/1, healthiness/1, unconstrained/1, explore/1, wealth/1, fight/1, abstinence/1, safety/1) as (incremental, dynamic).	

/*** Objectives Hierachy ***/
objective(X) :- success(X).
objective(X) :- healthiness(X).
objective(X) :- unconstrained(X).
objective(X) :- explore(X).
success(X) :- wealth(X).
success(X) :- fight(X).
healthiness(X) :- abstinence(X).
healthiness(X) :- safety(X).