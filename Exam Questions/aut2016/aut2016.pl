tr([[]|_], []).
tr(Ms, [Ts|Tss]) :-
        first_rest(Ms, Ts, Ms1),
        tr(Ms1, Tss).

first_rest([], [], []).
first_rest([[F|Os]|Rest], [F|Fs], [Os|Oss]) :-
        first_rest(Rest, Fs, Oss).
