%Q1
mylast([X], X).
mylast([_|Xs], Y)  :- mylast(Xs, Y).

%Q2
mybutlast([X,_], X).
mybutlast([_|Xs], Y) :- mybutlast(Xs, Y).

%Q3
elementat([X|_], 1, X).
elementat([_|Xs], I, Y) :- I1 is I-1, elementat(Xs,I1,Y).

%Q4
mylength([], 0).
mylength([_|Xs], Y+1) :- mylength(Xs, Y).

%Q5
myreverse([], Z, Z).
myreverse([X|Xs], Y, Z) :- myreverse(Xs, Y, [X|Z]).

%Q6
palindrome(Xs) :- myreverse(Xs, Ys, []), Xs==Ys.

