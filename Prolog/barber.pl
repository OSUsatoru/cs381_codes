
shaves(barber,X) :- male(X), \+(shaves(X,X)).

male(john). 
male(barber).
