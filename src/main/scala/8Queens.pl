:- use_module(library(clpfd)).

eight_queens(Qs) :-
        length(Qs, 8),
        Qs ins 1..8,
        no_attack(Qs).

no_attack([]).
no_attack([Q|Qs]) :-
        no_attack(Qs, Q, 1),
        no_attack(Qs).

no_attack([], _, _).
no_attack([Q|Qs], Q0, D0) :-
        Q0 #\= Q,
        abs(Q0 - Q) #\= D0,
        D1 #= D0 + 1,
        no_attack(Qs, Q0, D1).