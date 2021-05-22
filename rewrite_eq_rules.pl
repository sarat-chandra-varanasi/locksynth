
get_node_terms([node(var(X))|T], [X|R]) :-
		get_node_terms(T, R).

get_node_terms([], []).

get_key_terms([key(var(X))|T], [X|R]) :-
		get_key_terms(T, R).

get_key_terms([], []).

gen_eq_rule((Rule, Arg), []) :-
      dom_of(Arg, Rule, Domain),
      Domain \= node, Domain \= key.

gen_eq_rule((Rule, Arg), EqRule) :-
	  Rule = rule(head(Head), body(Body)),
	  dom_of(Arg, Body, node),
	  generate_symbol(node, X),
	  Head =.. [Name|Args],
	  replace(Arg, X, Args, Args1),
	  RewriteHead =.. [Name|Args1],
	  EqRule = rule(head(RewriteHead), body([Head, eq_node(X, Arg)])).

gen_eq_rule((Rule, Arg), EqRule) :-
	  Rule = rule(head(Head), body(Body)),
	  dom_of(Arg, Body, key),
	  generate_symbol(key, X),
	  Head =.. [Name|Args],
	  replace(Arg, X, Args, Args1),
	  RewriteHead =.. [Name|Args1],
	  EqRule = rule(head(RewriteHead), body([Head, eq_key(X, Arg)])).

gen_eq_rules(Rule, EqRules) :-
     Rule = rule(head(Head), body(Body)),
     Head =.. [_|Args],
     pair(Rule, Args, Pairs),
     maplist(gen_eq_rule, Pairs, EqRules).