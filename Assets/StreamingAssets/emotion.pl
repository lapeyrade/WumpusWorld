:- multifile [emotion/2].

:- table emotion/2 as incremental.

%%%%% ONTOLOGY EMOTION %%%%%
emotion(Id):- bad_emotion(Id).
emotion(Id):- good_emotion(Id).

bad_emotion(Id):- fear(Id).
bad_emotion(Id):- anger(Id).
bad_emotion(Id):- sadness(Id).
bad_emotion(Id):- disgust(Id).
bad_emotion(Id):- surprise(Id).
good_emotion(Id):- surprise(Id).
good_emotion(Id):- joy(Id).

fear(Id):- emotion(Id, fear).
anger(Id):- emotion(Id, anger).
sadness(Id):- emotion(Id, sadness).
disgust(Id):- emotion(Id, disgust).
surprise(Id):- emotion(Id, surprise).
joy(Id):- emotion(Id, joy).
