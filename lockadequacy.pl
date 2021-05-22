

default_window(Operation, Block, Rules) :- 
       ds(DS),
       code(DS, Operation, _, Block, Precondition, _, _),
       get_nodes(Precondition, Nodes),
       gen_lock_rules(Nodes, Precondition, Rules).


get_nodes(Precondition, Nodes2) :-
        maplist(get_nodes_helper, Precondition, Nodes),
        flatten(Nodes, Nodes1),
        remove_duplicates(Nodes1, Nodes2).

get_nodes_helper(Predicate, Nodes) :-
         lint_negation(Predicate, Linted),
         signature(Linted, Domains),
         Linted =.. [_|Args],
         pick_domain_vars(node, Args, Domains, Nodes).

pick_domain_vars(Domain, [H|T], [Domain|D], [H|R]) :-
            pick_domain_vars(Domain, T, D, R), !.

pick_domain_vars(Domain, [H|T], [Domain1|D], R) :-
            Domain \== Domain1, !, 
            pick_domain_vars(Domain, T, D, R).

pick_domain_vars(_, [], [], []).


gen_lock_rules([H|T], Precondition, [Rule|R]) :-
        % maplist(to_var, Precondition, PreFormatted),
        Rule = rule(head(locked(H)),body(Precondition)),
        gen_lock_rules(T, Precondition, R).

gen_lock_rules([], _, []).

check_adequacy(Operation, Block, Success) :-
      reset_counts,
      time_chain(2, Chain),
      gen_interference_theory(T),
      Rule = rule(head(Rulename), _),
      default_window(Operation, Block, Rs),
      ds(DS),
      code(DS, Operation, _, Block, Pre, _, _),
      generate_symbol(time, Time1),
      generate_symbol(time, Time2),
      add_time_list(Time1, Pre, PreFmt1),
      % maplist(to_var, PreFmt, PreFmt1),
      get_interference_head_and_domains(Operation, Block, Pre, Head, DomainVars),
      Head =.. [_|Vars],
      append(Vars, [Time1], Vars2),
      ConditionHead =.. [condition|Vars2],
      CondHead =.. [condition|Vars],
      Condition = rule(head(ConditionHead),body(PreFmt1)),
      add_time_unconditional_constant(t1, CondHead, Condt1),
      add_time_unconditional_constant(t2, CondHead, Condt2),
      Falsify = rule(head(falsify),body([time(t1), time(t2), 
                      next_time(t1, t2), Condt1, not(Condt2)])),
      Constraint = constraint([not(falsify)]),
      append(T, Rs, F),
      append(F, [Condition, Falsify, Constraint], F1),
      append(F1, Chain, F2),
      write_rules(F2, S),
      rules_str(S, S1),
      term_string(Operation, Ops),
      string_concat(Ops, "_", Ops1),
      term_string(Block, Bs),
      string_concat(Ops1, Bs, OperationBlock),
      string_concat('adequacy_', OperationBlock, Adequacy),
      add_to_base_theory(Adequacy, S1),
      solve_models(Adequacy, one, Models),
      (Models \= [] -> Success = false ; Success = true).

