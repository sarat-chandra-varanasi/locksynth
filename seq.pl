
domain(node).
domain(time).
domain(num).
domain(key).

background_theory(node, node_theory).
background_theory(num, num_theory).


operation(list, insert).
operation(list, delete).

operation(ext_tree, insert_ext_tree).
operation(ext_tree, delete_ext_tree).

primitive_write_step(list, link).
primitive_write_step(ext_tree, link_left).
primitive_write_step(ext_tree, link_right).

modifies(list, x, link(x,y)).
modifies(ext_tree, x, link_left(x,y)).
modifies(ext_tree, x, link_right(x,y)).

causes(list, edge(x,y), link(x,y)).
causes(ext_tree, left(x,y), link_left(x,y)).
causes(ext_tree, right(x,y), link_right(x,y)).


symbol_prefix(node, node_).
symbol_prefix(time, t).
symbol_prefix(key, k).
symbol_prefix(num, n).

node_property(edge).
node_property(key).

time_dependent(edge, 3).
time_dependent(list, 2).
time_dependent(suffix, 2).
time_dependent(reachable, 2).
time_dependent(present, 2).

time_dependent(left, 3).
time_dependent(right, 3).
time_dependent(external, 2).
time_dependent(internal, 2).
time_dependent(parent, 3).

constants(h, node).
constants(t, node).
constants(kh, num).
constants(kt, num).
constants(target, node).
constants(ktarget, num).
constants(nil, node).
constants(root, node).
constants(internal, node).
constants(kinternal, num).

terminal_node_symbol(t).
terminal_node_symbol(nil).

signature(list, 1, [time]).
signature(suffix, 2, [node, time]).
signature(reachable, 2, [node, time]).
signature(admissible, 2, [node, time]).
signature(edge, 3, [node, node, time]).
signature(key, 2, [node, num]).
signature(admissible, 1, [time]).
signature(present, 2, [key, time]).
signature(link, 3, [node, node, time]).
signature(read, 2, [node, time]).
signature(lt, 2, [num, num]).
signature(descendant, 4, [node, node, constant, time]).
signature(descendant, 3, [node, node, time]).
signature(left, 3, [node, node, time]).
signature(right,  3, [node, node, time]).	
signature(eq_node, 2, [node, node]).
signature(not_eq_node, 2, [node, node]).
signature(eq_num, 2, [num, num]).
signature(not_eq_num, 2, [num, num]).
signature(gt, 2, [num, num]).
signature(eq, 2, [node, node]).


signature(link_left, 3, [node, node, time]).
signature(link_right, 3, [node, node, time]).
signature(left, 3, [node, node, time]).
signature(right, 3, [node, node, time]).
signature(descendant, 4, [node, node, constant, time]).
signature(external, 2, [node, time]).
signature(internal, 2, [node, time]).
signature(parent, 3, [node, node, time]).

signature(reachable(_), [node]).
signature(key(_, _), [node, num]).
signature(edge(_,_), [node, node]).
signature(suffix(_), [node]).
signature(link(_, _), [node, node]).
signature(lt(_,_), [num, num]).
signature(present(_), [num]).
signature(external(_), [node]).
signature(internal(_), [node]).
signature(left(_,_), [node, node]).
signature(right(_,_),[node, node]).
signature(eq_node(_, _), [node, node]).
signature(not_eq_node(_, _), [node, node]).
signature(eq_num(_, _), [num, num]).
signature(not_eq_num(_, _), [num, num]).
signature(gt(_,_), [num, num]).
signature(parent(_,_), [node, node]).

signature(descendant(_, _, left), [node, node]).
signature(descendant(_, _, right), [node, node]).

max_steps(2).

% domains are never preconditions
% preconditions are time-dependent predicates
preconditions(reachable).
preconditions(admissible).
preconditions(edge).
preconditions(present).
preconditions(descendant).
preconditions(external).
preconditions(internal).

write_operation(list, insert, [node(target)]).
write_operation(list, delete, [node(target)]).

write_operation(ext_tree, insert_ext_tree, [node(target)]).
write_operation(ext_tree, delete_ext_tree, [node(target)]).

:- dynamic ds/1.

% time is implicit
code(list, insert, [target], block1, 
     [reachable(x), edge(x, y), not(reachable(target)), key(x, kx), key(y, ky), 
      key(target, ktarget), lt(kx, ktarget), lt(ktarget, ky)],
     [link(x,target),link(target,y)],
     [reachable(target)]
    ).


code(list, delete, [target], block1,
     [reachable(x), edge(x, target), edge(target, y) , key(x,kx), 
      key(y, ky), key(target, ktarget), lt(kx, ktarget), lt(ktarget, ky)],
     [link(x,y)],
    [not(reachable(target))]  
   ).

next_node(list,x,(y, [edge(x, y)])).
end_node(list, t).
next_node(list, t, t).


start_node(list, h).
start_node(ext_tree, root).
start_node(ext_tree, root).
end_node(ext_tree, nil).
next_node(ext_tree,x,(y,[key(x, kx), key(ktarget, ktarget), left(x, y), lt(ktarget, kx)])).
next_node(ext_tree,x,(y,[key(x, kx), key(ktarget, ktarget), right(x, y), lt(kx, ktarget)])).
next_node(ext_tree, nil, nil).

code(ext_tree, insert_ext_tree, [target], block1, [reachable(x),  parent(x, z), left(x, y), key(x, kx), key(y, ky), not(reachable(internal)), not(reachable(target)), external(y), key(target, ktarget), 
          lt(ktarget, kx), lt(ktarget, ky)], [link_left(internal, target), link_right(internal, y), link_left(x, internal)], []).

code(ext_tree, insert_ext_tree, [target], block2, [reachable(x), parent(x, z), left(x, y), key(x, kx), key(y, ky), not(reachable(internal)), not(reachable(target)), external(y), key(target, ktarget), 
          lt(ktarget, kx), lt(ky, ktarget)], [link_right(internal, target), link_left(internal, y), link_left(x, internal)], []).


code(ext_tree, insert_ext_tree, [target], block3, [reachable(x), parent(x,z), right(x, y), key(x, kx), key(y, ky), key(target, ktarget),
           lt(kx, ktarget), lt(ktarget, ky), not(reachable(target)), not(reachable(internal))], 
           [link_right(x, internal), link_left(internal, target), link_right(internal,y)], []).

code(ext_tree, insert_ext_tree, [target], block4, [reachable(x), parent(x,z), right(x, y), key(x, kx), key(y, ky), key(target, ktarget),
          lt(kx, ktarget), lt(ky, ktarget), not(reachable(target)), not(reachable(internal))], 
          [link_right(x, internal), link_left(internal,y), link_right(internal,target)], []).


code(ext_tree, delete_ext_tree, [target], block1, [reachable(x), left(x, y), left(y, z), right(y, z1), external(z), external(z1)], [link_left(x,z1)], []).
code(ext_tree, delete_ext_tree, [target], block2, [reachable(x), left(x, y), right(y, z), left(y, z1), external(z), external(z1)], [link_left(x,z1)], []).
code(ext_tree, delete_ext_tree, [target], block3, [reachable(x), right(x, y), left(y, z), right(y,z1),external(z), external(z1)], [link_right(x,z1)], []).
code(ext_tree, delete_ext_tree, [target], block4, [reachable(x), right(x, y), right(y, z), left(y,z1), external(z), external(z1)], [link_right(x,z1)], []).


max_depth(4).

class_ebst([node(a), node(b), node(c), node(d), node(e), node(f), node(nil), left(root, a), right(root, b),
            left(a, c), right(a, d), left(c, nil), right(c, nil), left(d, nil), right(d, nil), 
            left(b, e), right(b, f), left(e, nil), right(e, nil), left(f, nil), right(f, nil), key(a, ka), key(b, kb), key(c, kc), key(d, kd), key(e, ke), key(f, kf),  num(ka), 
            num(kb), num(kc), num(kd), num(ke), num(kf)]).
class(ext_tree, [node(a), node(b), node(c), node(d), node(e), node(f), node(nil), left(root, a), right(root, b),
            left(a, c), right(a, d), left(c, nil), right(c, nil), left(d, nil), right(d, nil), 
            left(b, e), right(b, f), left(e, nil), right(e, nil), left(f, nil), right(f, nil), key(a, ka), key(b, kb), key(c, kc), key(d, kd), key(e, ke), key(f, kf),  num(ka), 
            num(kb), num(kc), num(kd), num(ke), num(kf)]).

class(list, [node(a), node(b), node(c), node(d), edge(a, b), edge(b, c), edge(d, kd), key(a, ka), key(b, kb), key(c, kc), key(d, kd),
            num(ka), num(kb), num(kc), num(kd)]).


class_list([node(a), node(b), node(c), node(d), edge(a, b), edge(b, c), edge(d, kd), key(a, ka), key(b, kb), key(c, kc), key(d, kd),
            num(ka), num(kb), num(kc), num(kd)]).

invariant(list, [list]).
invariant(ext_tree, [not(violate_order), not(ext_tree)]).


time_dependent_node_property(list, edge).
time_dependent_node_property(ext_tree, left).
time_dependent_node_property(ext_tree, right).

time_dependent_node_property(int_tree, left).
time_dependent_node_property(int_tree, right).
