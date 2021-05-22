eq_node(X, Y) :- node(X), node(Y), not not_eq_node(X, Y).
not_eq_node(X, Y) :- node(X), node(Y), not eq_node(X, Y).

eq_node(X, Y) :- node(X), node(Y), eq_node(Y, X).
not_eq_node(X, Y) :- node(X), node(Y), not_eq_node(Y, X).


eq_node(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), eq_node(Z, Y).
