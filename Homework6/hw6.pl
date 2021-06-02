/* Team Members: Lance Adriano, Srikar Valluri, Satoru Yamamoto, and Alex Nguyen */

/* Exercise 1 (Srikar, Lance, Satoru) */

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216).

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

schedule(N, P, T) :- enroll(N, X), where(X, P), when(X, T).

usage(P, T) :- where(X, P), when(X, T).

conflict(X, Y) :- where(X, R), where(Y, R), when(X, T), when(Y, T), X \= Y.

meet(X, Y) :- enroll(X, C), enroll(Y, C), X \= Y.
meet(X, Y) :- enroll(X, C1), enroll(Y, C2), X \= Y,
              where(C1, R), where(C2, R),
              when(C1, T1), when(C2, T2), T2 =:= T1+1.


/* Exercise 2 (Lance, Alex, Satoru) */

member(X,[X|_]).
member(X,[_|Y]) :- member(X,Y).

append([], L, L).
append([X|L1], L2, [X|L3]) :- append(L1, L2, L3).

rdup([], []).
rdup([X|L], M) :- member(X, L), !, rdup(L, M).
rdup([X|L], [X|M]) :- rdup(L, M).

flat([], []).
flat([X|Xs], Y) :- flat(X, Y2), flat(Xs, Y3), append(Y2, Y3, Y).
flat(X, [X]).

project(_, [], []).
project([], _, []).