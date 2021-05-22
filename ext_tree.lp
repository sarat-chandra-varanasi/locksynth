eq_node(X, X) :- node(X).

eq_node(X, Y) :- node(X), node(Y), not not_eq_node(X, Y).
not_eq_node(X, Y) :- node(X), node(Y), not eq_node(X, Y).

eq_node(X, Y) :- node(X), node(Y), eq_node(Y, X).
not_eq_node(X, Y) :- node(X), node(Y), not_eq_node(Y, X).


eq_node(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), eq_node(Z, Y).


lt(X, Y) :- node(X), node(Y), node(Z), eq_node(Y, Z), lt(X, Z).
lt(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), lt(Z, Y).


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

key(X) :- num(X).

node(nil).
node(root).
root(root).
root(root, T) :- time(T).
root(X, T) :- time(T), root(Y, T), eq_node(Y, X).
:- time(T), root(X, T), root(Y, T), not_eq_node(X, Y).

tree(T) :- time(T), node(X), root(X), node(Y), node(Z), not_eq_node(X, Y), not_eq_node(X, Z), left(X, Y, T), right(X, Z, T), tree(Y, T), tree(Z, T).
tree(X, T) :- time(T), node(X), node(Y), node(Z), not_eq_node(X, Y), not_eq_node(X, Z), left(X, Y, T), right(X, Z, T), tree(Y, T), tree(Z, T).
tree(nil, T) :- time(T).


reachable(X) :-  node(X), root(X).
reachable(X) :-  node(X), node(Y), reachable(Y), left(Y, X).
reachable(X) :-  node(X), node(Y), reachable(Y), right(Y, X).
reachable(X) :-  node(X), node(Y), reachable(Y), eq_node(X, Y).


:- time(T), not tree(T). 

reachable(X, T) :- time(T), node(X), root(X, T).
reachable(X, T) :- time(T), node(X), node(Y), reachable(Y, T), left(Y, X, T).
reachable(X, T) :- time(T), node(X), node(Y), reachable(Y, T), right(Y, X, T).
reachable(X, T) :- time(T), node(X), node(Y), reachable(Y, T), eq_node(X, Y).


descendant(X, Y, left, T) :- time(T), node(X), node(Y), left(X, Y, T).
descendant(X, Y, right, T) :- time(T), node(X), node(Y), right(X, Y, T).
descendant(X, Y, T) :- time(T), node(X), node(Y), descendant(X, Y, left, T).
descendant(X, Y, T) :- time(T), node(X), node(Y), descendant(X, Y, right, T).
descendant(X, Z, left, T) :- time(T), node(X), node(Y), descendant(Y, Z, T), left(X, Y, T).
descendant(X, Z, right, T) :- time(T), node(X), node(Y), descendant(Y, Z, T), right(X, Y, T).


descendant(X, Y, left, T) :- time(T),  node(X), node(Y), node(Z), descendant(Z, Y, left, T), eq_node(X, Z).
descendant(X, Y, left, T) :- time(T), node(X), node(Y), node(Z), descendant(X, Z, left, T), eq_node(Y, Z).
descendant(X, Y, right, T) :- time(T), node(X), node(Y), node(Z), descendant(Z, Y, right, T), eq_node(X, Z).
descendant(X, Y, right, T) :- time(T), node(X), node(Y), node(Z), descendant(X, Z, right, T), eq_node(Y, Z).

descendant(X, Y, T) :- time(T), node(X), node(Y), node(Z), descendant(Z, Y, T), eq_node(X, Z).
descendant(X, Y, T) :- time(T), node(X), node(Y), node(Z), descendant(X, Z, T), eq_node(Y, Z).


violate_order :- time(T), node(X), node(Y), reachable(X, T), descendant(X, Y, left, T), num(KX), num(KY), key(X, KX), key(Y, KY), lt(KX, KY).
violate_order :- time(T), node(X), node(Y), reachable(X, T), descendant(X, Y, right, T), num(KX), num(KY), key(X, KX), key(Y, KY), lt(KY, KX).

:- violate_order.

:- time(T), node(X), node(Y), reachable(X, T), descendant(X, Y, T), descendant(Y, X, T).
:- time(T), node(X), reachable(X, T), descendant(X, Y, T), eq_node(X, Y).
:- time(T), node(X), node(Y), node(Z), Y != nil, Z != nil,  left(X, Y, T), left(X, Z, T), not_eq_node(Y, Z).
:- time(T), node(X), node(Y), node(Z), Y != nil, Z != nil, right(X, Y, T), right(X, Z, T), not_eq_node(Y, Z).
:- time(T), node(X), node(Y), node(Z), Y != nil, Z != nil,  left(X, Y, T), right(X, Z, T), eq_node(Y, Z).
:- not_eq_node(X, Y), key(X, KX), key(Y, KY), eq_num(KX, KY).
:- time(T), node(X), left(nil, X, T).
:- time(T), node(X), right(nil, X, T).
:-  eq_node(X, Y), key(X, KX), key(Y, KZ), not_eq_num(KX, KY).

not_ext_tree :- time(T), node(X), internal(X, T), left(X, nil, T).
not_ext_tree :- time(T), node(X), internal(X, T), right(X, nil, T).

:- not_ext_tree.

external(X) :-  node(X), reachable(X), left(X, nil), right(X, nil).
internal(X) :-  node(X), reachable(X), X != nil, not external(X).

external(X, T) :- time(T), node(X), reachable(X, T), left(X, nil, T), right(X, nil, T).
internal(X, T) :- time(T), node(X), reachable(X, T), X != nil, not external(X, T).

present(K, T) :- num(K), time(T), node(X), reachable(X, T), external(X, T), key(X, K).

node(target).
key(target, ktarget).
left(target, nil).
right(target, nil).


key(root, kroot).
num(ktarget).
num(kroot).
node(internal). key(internal, kinternal). num(kinternal).

not_eq_node(nil, X) :- node(X), nil != X.

left(X, Y, t1) :- node(X), node(Y), left(X, Y).
right(X, Y, t1) :- node(X), node(Y), right(X, Y).

left(X, Y, T1) :- node(X), node(Y), time(T1), time(T), next_time(T, T1), left(X, Y, T), not modifies_left(X, T).
right(X, Y, T1) :- node(X), node(Y), time(T1), time(T), next_time(T, T1), right(X, Y, T), not modifies_right(X, T).

locked(X) :- node(X), node(Y), eq_node(X, Y), locked(Y).

parent(X, Y, T) :- node(X), node(Y), time(T), left(Y, X, T).
parent(X, Y, T) :- node(X), node(Y), time(T), right(Y, X, T).

parent(X, Y) :- node(X), node(Y), left(Y, X).
parent(X, Y) :- node(X), node(Y),  right(Y, X).
