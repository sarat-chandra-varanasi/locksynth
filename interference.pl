

gen_interference_theory(Flattened) :-
        ds(DS),
        findall(Op, write_operation(DS,Op,_), Ops),
        maplist(gen_interference_rule_op, Ops, InterferenceTheory), !,
        flatten(InterferenceTheory, InterferenceTheory1),
        rewrite_primitives(Rewrites),
        append(InterferenceTheory1, Rewrites, Append),
        gen_constraints(Constraints),
        append(Append, Constraints, Append2),
        flatten(Append2, Flattened),
        write_to_file('Interference', Flattened).
 

gen_interference_rule_op(Op, Rules) :-
       ds(DS),
       findall(Block, code(DS, Op, _, Block, _, _, _), List), 
       gen_interference_rule_blocks(Op, List, Rules).


gen_interference_rule_blocks(Op, [Block|T], [Rule|Rules]) :-
         gen_interference_rule(Op, Block, Rule),
         gen_interference_rule_blocks(Op, T, Rules).

gen_interference_rule_blocks(_, [], []).

gen_interference_rule(Op, Block, Rule) :-
        ds(DS),
        code(DS, Op, _, Block, Pre, Steps, _),
        gen_interference_predicate(Op, Block, Pre, Predicate),
        gen_interference_effect(Op, Predicate, Steps, Effects),
        Rule = [Predicate, Effects].

op_blocks(OpBlocks2) :-
     ds(DS),
     findall(Op, code(DS, Op, _, _, _, _, _),Ops),
     blocks(Ops, OpBlocks),
     remove_duplicates(OpBlocks, OpBlocks1),
     flatten(OpBlocks1, OpBlocks2).

blocks([Op|T], [OpBlocks|R]) :-
    ds(DS),
    findall(Block, code(DS, Op, _, Block, _, _, _), Blocks),
    zip(Op, Blocks, OpBlocks),    
    blocks(T, R).

blocks([], []).
get_interference_heads(Heads) :-
          op_blocks(OpBlocks),
          get_interference_heads_helper(OpBlocks, Heads).

get_interference_heads_helper([(Op,Block)|T], [(Head,Domains)|R]) :-
           ds(DS),
           code(DS,Op, _, Block, Pre, _, _),
           get_interference_head_and_domains(Op, Block, Pre, Head, Domains), !,
           get_interference_heads_helper(T, R).

get_interference_heads_helper([], []).


get_interference_head_and_domains(Operation, Block, Pre, Head, DomainVars) :-
     term_string(Operation, Op),
     term_string(Block, Bl),
     string_concat(Op, Bl, OpBlock),
     string_concat('interfere_', OpBlock, InterfereString),
     term_string(Interfere, InterfereString),
     conjunct_vars(Pre, Vars),
     %generate_symbol(time, TimeSymbol),
     %append(Vars, [TimeSymbol], HeadVars),
     Head =.. [Interfere|Vars],
     conjunct_domainvars(Pre, DomainVars).

gen_interference_predicate(Operation, Block, Pre, Predicate) :-
     term_string(Operation, Op),
     term_string(Block, Bl),
     string_concat(Op, Bl, OpBlock),
     string_concat('interfere_', OpBlock, InterfereString),
     term_string(Interfere, InterfereString),
     conjunct_vars(Pre, Vars),
     generate_symbol(time, TimeSymbol),
     append(Vars, [TimeSymbol], HeadVars),
     Head =.. [Interfere|HeadVars],
     neg_term(Interfere, NegInterfere),
     NegHead =.. [NegInterfere|HeadVars],
     append(Pre, [time(TimeSymbol), not(NegHead)], Body),
     append(Pre, [time(TimeSymbol), not(Head)], NegBody),
     conjunct_domainvars(Pre, DomainVars),
     add_time_list(TimeSymbol, Body, Body1),
     add_time_list(TimeSymbol, NegBody, NegBody1),
     append(DomainVars, Body1, Body2),
     append(DomainVars, NegBody1, NegBody2),
     Predicate = [rule(head(Head),body(Body2)),
                  rule(head(NegHead),body(NegBody2))].

gen_interference_effect(Operation, Predicate, Steps, Effects) :-
     Predicate = [rule(head(Head),_)|_],
     link_program_steps(Steps, Head, Effects).

link_program_steps([H|T], InterfereHead, [Effect|R]) :-
         rewrite_interference_name(H, H1),
         copy_timevar(InterfereHead, H1, EffectHead),
         Effect = rule(head(EffectHead), body([InterfereHead])),
         link_program_steps(T, InterfereHead, R).

link_program_steps([], _, []).

gen_constraints(Constraints) :-
    get_interference_heads(Heads),
    gen_constraints_between_terms(C1),
    gen_within_terms_rec(Heads, C2),
    append(C1, C2, Constraints).

gen_within_terms_rec([H|T], Constraints) :-
       H = (Head, Domains),
       gen_pairwise_constraints_within_term(Head, Domains, C), 
       gen_within_terms_rec(T, R),
       append(C, R, Constraints).

gen_within_terms_rec([], []).

gen_pairwise_constraints_within_term(InterferenceHead, Domains, Constraints) :-
        maplist(domain_variant, Domains, Variants),
        rename_vars(InterferenceHead, Variants, HeadVariant),
        append(Domains, Variants, L1),
        generate_symbol(time, TimeSymbol),
        add_time_unconditional(TimeSymbol, InterferenceHead, InterferenceHead1),
        add_time_unconditional(TimeSymbol, HeadVariant, HeadVariant1),
        append(L1, [time(TimeSymbol), InterferenceHead1, HeadVariant1], Base),
        gen_constraints_within(Base, Domains, Variants, Constraints).
        

gen_constraints_within(Base, [Term1|T1], [Term2|T2],[Rule|R]) :- 
          Term1 =.. [Domain, Var1],
          Domain \= node, Domain \= num,
          Term2 =.. [_,Var2],
          append(Base, [diff_term(Var1,Var2)], Body),
          Rule = constraint(Body),
          gen_constraints_within(Base, T1, T2, R).

gen_constraints_within(Base, [Term1|T1], [Term2|T2],[Rule|R]) :- 
          Term1 =.. [node, Var1],
          Term2 =.. [_,Var2],
          append(Base, [not_eq_node(Var1,Var2)], Body),
          Rule = constraint(Body),
          gen_constraints_within(Base, T1, T2, R).

gen_constraints_within(Base, [Term1|T1], [Term2|T2],[Rule|R]) :- 
          Term1 =.. [num, Var1],
          Term2 =.. [_,Var2],
          append(Base, [not_eq_num(Var1,Var2)], Body),
          Rule = constraint(Body),
          gen_constraints_within(Base, T1, T2, R).


gen_constraints_within(_, [], [], []).


gen_constraint_between_terms(Head1, Domains1, Head2, Domains2, Rule) :-
    append(Domains1, Domains2, L1),
    generate_symbol(time, TimeSymbol),
    add_time_unconditional(TimeSymbol, Head1, TimeHead1),
    add_time_unconditional(TimeSymbol, Head2, TimeHead2),
    append(L1, [time(TimeSymbol), TimeHead1, TimeHead2], Body),
    Rule = constraint(Body).

gen_constraints_between_terms(Constraints) :-
          get_interference_heads(Heads),
          gen_constraints_between_terms_helper(Heads, Heads, Constraints).

gen_constraints_between_terms_helper([H|T], [H|T], Constraints) :-
           gen_constraints_between_terms_helper([H|T], T, Constraints).

gen_constraints_between_terms_helper([H|T], [H1|T1], [Rule|R]) :-
            H \= H1,
            H = (Head1, Domains1),
            H1 = (Head2, Domains2),
            %same_timevar(Head1, Head2, Head3),
            gen_constraint_between_terms(Head1, Domains1, Head2, Domains2, Rule),
            gen_constraints_between_terms_helper([H|T],T1,R).

gen_constraints_between_terms_helper([H|T], [], Constraints) :-
            gen_constraints_between_terms_helper(T, T, Constraints).

gen_constraints_between_terms_helper([], [], []).

rewrite_interference_name(Term, Term2) :-
            Term =.. [Name|List],
            term_string(Name, NameStr),
            string_concat(NameStr, '_interfere', NameStr2),
            term_string(Name2, NameStr2),
            Term2 =.. [Name2|List].

rewrite_primitives(Rewrites) :-
     ds(DS),
     findall(S, primitive_write_step(DS,S), Primitives),
     rewrite_primitives_helper(Primitives, Rewrites).

rewrite_primitives_helper([H|T], R1) :-
         rewrite_interference_name(H, Hr),
         signature(H, Arity, _),
         Arity1 is Arity - 1,
         length(Vars, Arity1),
         Term =.. [H|Vars],
         ds(DS),
         causes(DS, Effect, Term), % also captures dataflow thanks to prolog unification
         modifies(DS, ModifiedMem, Term),
         generate_symbol(time, TimeSymbol),
         % add_time_unconditional(TimeHead1, Hr, Hr1).
         Term1 =.. [Hr|Vars],
         add_time_unconditional(TimeSymbol, Term1, Term2),
         term_domainvars(Term, DomainVars),
         generate_symbol(time, NextTimeSymbol),
         add_time_unconditional(NextTimeSymbol, Effect, Effect2),
         append(DomainVars, [time(TimeSymbol), time(NextTimeSymbol), next_time(TimeSymbol, NextTimeSymbol), Term2, not(locked(ModifiedMem))], Body),
         Rewrite = rule(head(Effect2),body(Body)),
         gen_causal_rule(H, Hr, CausalRule),
         append([Rewrite, CausalRule], R, R1), !,
         rewrite_primitives_helper(T, R).

gen_causal_rule(H, Hr, Causal) :-
       signature(H, Arity, _),
       Arity1 is Arity - 1,
       length(Vars, Arity1),
       Term =.. [H|Vars],
       ds(DS),
       modifies(DS, Memory, Term),
       term_domainvars(Term, DomainVars),
       generate_symbol(time, TimeSymbol),
       append(Vars, [TimeSymbol], Vars1),
       Term1 =.. [Hr|Vars1],
       causal_head_name(Term, Name),
       append(DomainVars, [time(TimeSymbol), Term1, not(locked(Memory))], CausalBody),
       Head =.. [Name, Memory, TimeSymbol],
       Causal = rule(head(Head), body(CausalBody)).


causal_head_name(Term, Name) :-
        ds(DS),
        causes(DS, Term2, Term),
        Term2 =.. [Property|_],
        term_string(Property, Ps),
        string_concat('modifies_', Ps, NameString),
        term_string(Name, NameString).

rewrite_primitives_helper([], []).