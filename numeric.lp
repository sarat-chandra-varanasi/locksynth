
% law of trichotomy
lt(X, Y) :- num(X), num(Y), not eq(X, Y), not gt(X, Y).
eq(X, Y) :- num(X), num(Y), not lt(X, Y), not gt(X, Y).
gt(X, Y) :- num(X), num(Y), not eq(X, Y), not lt(X, Y).


lt(X, Y) :- num(X), num(Y), gt(Y, X).
gt(X, Y) :- num(X), num(Y), lt(Y, X).

% transitivity of lt
lt(X, Y) :- num(X), num(Y), num(Z), lt(X, Z), lt(Z, Y).
 
% transitivity of eq
eq(X, Y) :- num(X), num(Y), num(Z), eq(X, Z), eq(Z, Y).

% symmetric eq
eq(X, Y) :- num(X), num(Y), eq(Y, X).




