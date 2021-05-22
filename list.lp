
eq_node(X, X) :- node(X).

eq_node(X, Y) :- node(X), node(Y), not not_eq_node(X, Y).
not_eq_node(X, Y) :- node(X), node(Y), not eq_node(X, Y).

eq_node(X, Y) :- node(X), node(Y), eq_node(Y, X).
not_eq_node(X, Y) :- node(X), node(Y), not_eq_node(Y, X).


eq_node(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), eq_node(Z, Y).

eq_num(X, Y) :- num(X), num(Y), num(Z), eq_num(X, Z), eq_num(Z, Y).

eq_num(X, X) :- num(X).
eq_num(X, Y) :- num(X), num(Y), eq_num(Y, X).

not_eq_num(X, Y) :- num(X), num(Y), not_eq_num(Y, X).

% law of trichotomy
lt(X, Y) :- num(X), num(Y), not eq_num(X, Y), not gt(X, Y).
eq_num(X, Y) :- num(X), num(Y), not lt(X, Y), not gt(X, Y).
gt(X, Y) :- num(X), num(Y), not eq_num(X, Y), not lt(X, Y).


lt(X, Y) :- num(X), num(Y), gt(Y, X).
gt(X, Y) :- num(X), num(Y), lt(Y, X).

% transitivity of lt
lt(X, Y) :- num(X), num(Y), num(Z), lt(X, Z), lt(Z, Y).
 
% transitivity of eq
eq_num(X, Y) :- num(X), num(Y), num(Z), eq_num(X, Z), eq_num(Z, Y).

% symmetric eq
eq_num(X, Y) :- num(X), num(Y), eq_num(Y, X).

:- num(X), num(Y), lt(X, Y), eq_num(X, Y).
:- num(X), num(Y), gt(X, X), eq_num(X, Y).

:- node(X), node(Y), eq_node(X, Y), num(K1), num(K2), key(X, K1), key(Y, K2), not_eq_num(K1, K2).

reachable(h).
reachable(X) :- node(X), reachable(Y), edge(Y, X).


reachable(X, T) :- node(X), time(T), node(Y), eq_node(X, Y), reachable(Y, T).
present(K, T) :-  time(T), num(K), num(K1), eq_num(K, K1), present(K1, T). 
list(X, T) :- node(X), time(T), eq_node(X, Y), list(Y, T).
edge(X, Y, T) :- node(X), node(Y), time(T), node(Z), eq_node(X, Z), edge(Z, Y, T).
edge(X, Y, T) :- node(X), node(Y), time(T), node(Z), eq_node(Y, Z), edge(X, Z, T).
lt(X, Y) :- num(X), num(Y), eq_num(X, Z), lt(Z, Y).
lt(X, Y) :- num(X), num(Y), eq_num(Y, Z), lt(X, Z).
gt(X, Y) :- num(X), num(Y), eq_num(X, Z), gt(Z, Y).
gt(X, Y) :- num(X), num(Y), eq_num(Y, Z), gt(X, Z).

% time(t0).
% time(t1).
% next_time(t0, t1).

edge(X, Y, t1) :- node(X), node(Y), edge(X, Y).


lst(T) :- time(T), edge(h, t, T), num(K1), num(K2), key(h, K1), key(t, K2), lt(K1, K2).
lst(T) :- time(T), node(X), edge(h, X, T), not_eq_node(h, X), num(K1), num(K2), key(h, K1), key(X, K2), lt(K1, K2), list(X, T). 
list(X, T) :- time(T), node(X), edge(X, Y, T), not_eq_node(X, Y), num(K1), num(K2), key(X, K1), key(Y, K2), lt(K1, K2), list(Y, T).
list(t, T) :- time(T).

 
reachable(h, T) :- time(T).
reachable(X, T) :- time(T), node(X), node(Y), edge(Y, X, T), reachable(Y, T).

present(K, T) :- num(K), time(T), node(X), reachable(X, T), key(X, K).

% link(X, Y, T) :- node(X), node(Y), time(T), not edge(X, Y, T), not not_link(X, Y, T).
% not_link(X, Y, T) :- node(X), node(Y), time(T), not edge(X, Y, T), not link(X, Y, T).


:- node(X), node(Y), edge(X, Y, T), eq_node(X, Y).
:- node(X), node(Y), node(Z), edge(X, Y, T), edge(X, Z, T), not_eq_node(Y, Z).
:- node(X), node(Y), edge(X, Y, T), edge(Y, X, T).
:- node(X), edge(X, h, T).
:- node(X), edge(t, X, T).
:- node(X), node(Y), link(X, Y, 1).

modified(X, T) :- node(X), time(T), node(Y), link(X, Y, T).

edge(X, Y, T1) :- node(X), node(Y), time(T), time(T1), next_time(T, T1), link(X, Y, T).
edge(X, Y, T1) :- node(X), node(Y), time(T), time(T1), next_time(T, T1), edge(X, Y, T), not modified(X, T).

:- time(T), not lst(T).

node(h).
node(a).
node(b).
node(t).
num(kh).
num(ka).
num(kb).
num(kt).
edge(h, a).
edge(a, b).
edge(b, t).
key(h, kh).
key(a, ka).
key(b, kb).
key(t, kt).


node(target).
num(ktarget).
key(target, ktarget).


lt(kh, ktarget).
lt(ktarget, kt).
:- node(target), node(X), X != target, eq_node(target, X).
:- present(ktarget, t1).


