eq_node(X, X) :- node(X).

eq_node(X, Y) :- node(X), node(Y), not not_eq_node(X, Y).
not_eq_node(X, Y) :- node(X), node(Y), not eq_node(X, Y).

eq_node(X, Y) :- node(X), node(Y), eq_node(Y, X).
not_eq_node(X, Y) :- node(X), node(Y), not_eq_node(Y, X).


eq_node(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), eq_node(Z, Y).


lt(X, Y) :- node(X), node(Y), node(Z), eq_node(Y, Z), lt(X, Z).
lt(X, Y) :- node(X), node(Y), node(Z), eq_node(X, Z), lt(Z, Y).



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

:- num(X), lt(X, X).
:- num(X), gt(X, X).
% :- num(X), num(Y), X != Y, eq(X, Y).

key(X) :- num(X).

node(nil).
node(root).
root(root).
root(X) :- root(Y), eq_node(Y, X).
:- root(X), root(Y), not_eq_node(X, Y).

tree :- node(X), root(X), node(Y), node(Z), not_eq_node(X, Y), not_eq_node(X, Z), left(X, Y), right(X, Z), tree(Y), tree(Z).
tree(X) :- node(X), node(Y), node(Z), not_eq_node(X, Y), not_eq_node(X, Z), left(X, Y), right(X, Z), tree(Y), tree(Z).
tree(nil).

:- not tree. 

reachable(X) :- node(X), root(X).
reachable(X) :- node(X), node(Y), reachable(Y), left(Y, X).
reachable(X) :- node(X), node(Y), reachable(Y), right(Y, X).
reachable(X) :- node(X), node(Y), reachable(Y), eq_node(X, Y).


descendant(X, Y, left) :- node(X), node(Y), left(X, Y).
descendant(X, Y, right) :- node(X), node(Y), right(X, Y).
descendant(X, Y) :- node(X), node(Y), descendant(X, Y, left).
descendant(X, Y) :- node(X), node(Y), descendant(X, Y, right).
descendant(X, Y, left) :- node(X), node(Y), descendant(Y, Z), left(X, Y).
descendant(X, Y, right) :- node(X), node(Y), descendant(Y, Z), right(X, Y).


descendant(X, Y, left) :- node(X), node(Y), node(Z), descendant(Z, Y, left), eq_node(X, Z).
descendant(X, Y, left) :- node(X), node(Y), node(Z), descendant(X, Z, left), eq_node(Y, Z).
descendant(X, Y, right) :- node(X), node(Y), node(Z), descendant(Z, Y, right), eq_node(X, Z).
descendant(X, Y, right) :- node(X), node(Y), node(Z), descendant(X, Z, right), eq_node(Y, Z).

descendant(X, Y) :- node(X), node(Y), node(Z), descendant(Z, Y), eq_node(X, Z).
descendant(X, Y) :- node(X), node(Y), node(Z), descendant(X, Z), eq_node(Y, Z).


violate_order :- node(X), node(Y), reachable(X), descendant(X, Y, left), num(KX), num(KY), key(X, KX), key(Y, KY), lt(KX, KY).
violate_order :- node(X), node(Y), reachable(X), descendant(X, Y, right), num(KX), num(KY), key(X, KX), key(Y, KY), lt(KY, KX).

:- violate_order.

:- node(X), node(Y), reachable(X), descendant(X, Y), descendant(Y, X).
:- node(X), reachable(X), descendant(X, Y), eq_node(X, Y).
:- node(X), node(Y), node(Z), Y != nil, Z != nil,  left(X, Y), left(X, Z), not_eq_node(Y, Z).
:- node(X), node(Y), node(Z), Y != nil, Z != nil, right(X, Y), right(X, Z), not_eq_node(Y, Z).
:- node(X), node(Y), node(Z), Y != nil, Z != nil,  left(X, Y), right(X, Z), eq_node(Y, Z).
:- not_eq_node(X, Y), key(X, KX), key(Y, KY), eq(KX, KY).
:- node(X), left(nil, X).
:- node(X), right(nil, X).

external(X) :- node(X), reachable(X), left(X, nil), right(X, nil).
internal(X) :- node(X), X != nil, not external(X).


node(target).
key(target, ktarget).
key(root, kroot).
num(ktarget).

num(kroot).


condition :- reachable(X), left(X, Y),right(Y, Z),external(Z),eq_node(Z, target).

:- not condition.
node(a).
node(b).
node(c).
node(d).
node(e).
node(f).
node(nil).
left(root,a).
right(root,b).
left(a,c).
right(a,d).
left(c,nil).
right(c,nil).
left(d,nil).
right(d,nil).
left(b,e).
right(b,f).
left(e,nil).
right(e,nil).
left(f,nil).
right(f,nil).
key(a,ka).
key(b,kb).
key(c,kc).
key(d,kd).
key(e,ke).
key(f,kf).
num(ka).
num(kb).
num(kc).
num(kd).
num(ke).
num(kf).
