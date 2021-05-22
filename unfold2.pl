

unfold2(0, Terms) :-
     start(Start),
     find_rule(Start, Rule), 
     Rule = rule(head(Start), body(Body)),
     vars_of(Body, Vars2),
     gen_fresh_symbols(Vars2, Body, FreshVars),
     subst_fresh_vars_conjunct(FreshVars, Body, Body1),
     get_nonterminals(Body1, NonTerminals),
     set_diff(Body1, NonTerminals, Terms).   

unfold2_goals([(Pred, CurrentDepth)|T], Depth, [Terms|Terms2]) :-
     find_rule(Pred, Rule), 
     CurrentDepth < Depth,
     Rule = rule(head(Head), body(Body)),
     Head =.. [_|Vars],
     Pred =.. [_|Args],
     pair_symbol_vars(Args, Vars, Pairs),
     subst_fresh_vars_conjunct(Pairs, Body, Body1),
     vars_of(Body1, Vars2),
     gen_fresh_symbols(Vars2, Body1, Vars3),
     subst_fresh_vars_conjunct(Vars3, Body1, Body2),
     get_nonterminals(Body2, NonTerminals),
     set_diff(Body2, NonTerminals, Terms),
     NonTerminals \= [],
     CurrentDepth1 is CurrentDepth + 1,
     pair_list_value(NonTerminals, CurrentDepth1, Goals),
     append(T, Goals, T2),
     unfold2_goals(T2, Depth, Terms2).

:- dynamic count/1.

unfold2(Depth, [Terms|Terms2]) :-
	 Depth > 0, 
	 start(Start),
     find_rule(Start, Rule), 
     Rule = rule(head(Start),body(Body)),
     vars_of(Body, Vars2),
     gen_fresh_symbols(Vars2, Body, FreshVars),
     subst_fresh_vars_conjunct(FreshVars, Body, Body1),
     get_nonterminals(Body1, NonTerminals),
     set_diff(Body1, NonTerminals, Terms),
     pair_list_value(NonTerminals, 1, Goals),
     unfold2_goals(Goals, Depth, Terms2).   

  
unfold2_goals([(Pred, CurrentDepth)|T], Depth, Terms) :-
     find_rule(Pred, Rule), 
     CurrentDepth < Depth,
     Rule = rule(head(Head), body(Body)),
     Head =.. [_|Vars],
     Pred =.. [_|Args],
     pair_symbol_vars(Args, Vars, Pairs),
     subst_fresh_vars_conjunct(Pairs, Body, Body1),
     vars_of(Body1, Vars2),
     gen_fresh_symbols(Vars2, Body1, Vars3),
     subst_fresh_vars_conjunct(Vars3, Body1, Body2),
     get_nonterminals(Body2, NonTerminals),
     NonTerminals = [],
     unfold2_goals(T, Depth, Terms).   

 unfold2_goals([], _, []).

unfold2_goals([(Pred, Depth)|T], Depth, Terms2) :-
     unfold2_goals(T, Depth, Terms2).


ufold(Depth, T) :-
     unfold2(Depth, Terms), flatten(Terms, T).

