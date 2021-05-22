

% eq_node(X, Y) :- node(X), node(Y), not not_eq_node(X, Y).
% not_eq_node(X, Y) :- node(X), node(Y), not eq_node(X, Y).

% eq_node(X, Y) :- node(X), node(Y), eq_node(Y, X).
% not_eq_node(X, Y) :- node(X), node(Y), not_eq_node(Y, X).


% eq_node(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), eq_node(Z, Y).

% law of trichotomy
% lt(X, Y) :- num(X), num(Y), not eq(X, Y), not gt(X, Y).
% eq(X, Y) :- num(X), num(Y), not lt(X, Y), not gt(X, Y).
% gt(X, Y) :- num(X), num(Y), not eq(X, Y), not lt(X, Y).


% lt(X, Y) :- num(X), num(Y), gt(Y, X).
% gt(X, Y) :- num(X), num(Y), lt(Y, X).

% transitivity of lt
% lt(X, Y) :- num(X), num(Y), num(Z), lt(X, Z), lt(Z, Y).
 
% transitivity of eq
% eq(X, Y) :- num(X), num(Y), num(Z), eq(X, Z), eq(Z, Y).

% symmetric eq
% eq(X, Y) :- num(X), num(Y), eq(Y, X).

% :- num(X), lt(X, X).
% :- num(X), gt(X, X).
% :- num(X), num(Y), X != Y, eq(X, Y).

list :- 
  node(h), node(X),  edge(h, X), key(h, kh), num(kh), num(KX), key(X, KX), lt(kh, KX), suffix(X).
suffix(t).
suffix(X) :- 
   node(X), node(Y), edge(X, Y), num(KX), num(KY), key(X, KX), key(Y, KY), lt(KX, KY), suffix(Y).

:- node(X), edge(X, X).

definition(list).
domain(node).	
domain(num).
domain(time).
domain(key).

theory(node, node_theory).
theory(num, num_theory).
theory(time, time_theory).

time_dependent(edge, 3).
time_dependent(list, 1).
time_dependent(suffix, 2).
time_dependent(reachable, 2).


constant(h, node).
constant(t, node).
constant(kh, num).
constant(kt, num).


node(h).
node(t).
node(target).

key(h, kh).
key(target, ktarget).
key(t, kt).

num(kh).
num(kt).
num(ktarget).


reachable(h).
reachable(X) :- node(X), node(Y), reachable(Y), edge(Y, X).

% constraint([time(T), edge(X, Y, T), edge(X, Z, T), diff_term(Y, Z)]).

:- not list.

effect(edge(x, y), link(x, y)).
modifies(x, link(x, y)).
node_property(edge).
node_property(key).



% atmost one time dependent node property might be modified at any instant!
% node properties are exclusively functions of nodes
