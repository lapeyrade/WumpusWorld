:- table (action/1, interact/1, attack/1, shoot/1, move/1, moveback/1, bumpwall/1, pickup/1, discard/1, shootarrow/1) as (incremental, dynamic).

/*** Actions Hierachy ***/
action(X) :- interact(X).
action(X) :- attack(X).
action(X) :- move(X).
action(X) :- moveback(X).
action(X) :- bumpwall(X).
interact(X) :- pickup(X).
interact(X) :- discard(X).
attack(X) :- shoot(X).
shoot(X) :- shootarrow(X).