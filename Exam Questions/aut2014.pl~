parent([[P,S]|_], P, S).
parent([_|Xs], P, S) :- parent(Xs, P, S).

sibling([[P,S1]|Xs], S1, S2) :- parent(Xs, P, S2).
sibling([[P,S2]|Xs], S1, S2) :- parent(Xs, P, S1).
