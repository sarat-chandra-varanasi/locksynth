
select([H|T], T, H).
select([H|T], [H|R], R1) :-
        select(T, R, R1).

perm(List, R1) :-
	select(List, X, Y),
	perm(X, R),
	append(R,[Y], R1).

perm([], []).

infer_max_depth(MaxDepth) :-
      ds(DS),
      findall(Steps, code(DS, _, _, _, _, Steps, _), List),
      maplist(length, List, Lengths),
      max(Lengths, MaxDepth).

max(List, M) :- 
      List = [H|T],
      max_helper(List, H, M).

max_helper([H|T], P, M) :-
       H > P, 
       max_helper(T, H, M).

max_helper([H|T], P, M) :-
       H =< P, 
       max_helper(T, P, M).

max_helper([], M, M).

rename_steps(Steps, Rename, Rules) :-
     rename_steps(Steps, 0, Rename, Rules).

rename_steps([H|T], C, [H1|R], [Rule|Rules]) :-
      H =.. [Name|Args],
      term_string(C, Cs),
      string_concat("_", Cs, Suffix),
      string_concat(Name, Suffix, Rs),
      term_string(Rename, Rs),
      H1 =..[Rename|Args],
      C1 is C + 1,
      link_renamed_step(H, H1, Rule),
      rename_steps(T, C1, R, Rules).

link_renamed_step(Primitive, Renamed, Rule) :-
      reset_counts,
      generate_symbol(time, Time),
      add_time_unconditional(Time, Primitive, P),
      add_time_unconditional(Time, Renamed, R),
      Head = head(P),
      Body = body([R]),
      Rule = rule(Head, Body).

rename_steps([], _, [], []).

gen_program_step_rules(Steps, Pre, [Rule|R]) :-
	  reset_counts,
	  generate_symbol(time, Time),
	  add_time_list_constant(Time, Pre, Body),
	  Steps = [H|T],
	  add_time_unconditional_constant(Time, H, H1),
	  Rule = rule(head(H1), body(Body)),
	  gen_program_step_rules_helper(T, Time, H1, Body, R).

gen_program_step_rules_helper([H|T], PrevTime, Body, Pre, [Rule|R]) :-
	  generate_symbol(time, Time),
	  append([Body], [next_time(constant(PrevTime), constant(Time))], Body1),
	  append(Body1, Pre, Body2),
	  add_time_unconditional_constant(Time, H, H1),
	  Rule = rule(head(H1), body(Body2)),
	  gen_program_step_rules_helper(T, Time, H1, Pre, R).

gen_program_step_rules_helper([], _, _, _, []).

check_reorder(Op, Block) :-
      check_reorder(Op, Block, _).

check_reorder(Op, Block, Perm) :-
      ds(DS),
      code(DS, Op, _, Block, Pre, Steps, _),
      rename_steps(Steps, Rename, Rules),
      perm(Rename, Perm),
      check_perm(Op, Block, Perm, Pre, Rules, true).
     
check_perm(Op, Block, Steps, Pre, Rules, Success) :-
	  gen_reorder_theory(Op, Block, Steps, Pre, Rules1),
      append(Rules, Rules1, Rules2),
      write_rules(Rules2, RulesStr),
      rules_str(RulesStr, R1),
      term_string(Op, Ops),
      term_string(Block, Bs),
      reduce(['reorder', Ops, Bs], add_underscore, '', File),
      add_to_base_theory(File, R1),
      solve_models(File, one, Models),
      (Models = [] -> Success = false ; Success = true).

 
gen_reorder_theory(Op, Block, Steps, Pre, Rules) :-
      reset_counts,
      infer_max_depth(Depth),
      Depth1 is Depth + 1,
      time_chain(Depth1, TimeRules),
      reset_counts,
      gen_program_step_rules(Steps, Pre, ProgramStepRules),
      append(ProgramStepRules, TimeRules, Rules).
      
