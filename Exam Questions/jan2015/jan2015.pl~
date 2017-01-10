member(Y,[Y|_]).
member(Y,[_|Xs]) :- member(Y,Xs).

doublemember(Y,[Y|Xs]) :- member(Y,Xs).
doublemember(Y,[_|Xs]) :- doublemember(Y,Xs).
