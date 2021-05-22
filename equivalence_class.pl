% Find symbolic instances of data structure definition that satisfiy a relation
% unfold sytematically the definition of the data stucture
% we can find the instances ourselves (or) get it as input from user of the system
% every predicate used in a time-independent fashion

get_rule(Name, Rule) :-
      rule(Name, Head, Body).

      
equiv_class(Conjunct, Class) :-
     unfold_and_check(Conjunct, Class).


unfold_and_check(Conjunct, Class) :-
        unfold_and_check(Conjunct, Class, 0).


unfold_and_check(Conjunct, Class, Depth) :-
         %findall(Terms, unfold(Depth, Terms), List),
         %check_list(List, Conjunct).
         unfold(Depth, Terms),
         flatten(Terms, Flattened),
         check_term(Flattened, Conjunct).

unfold_and_check(Conjunct, Class, Depth) :-
        Depth1 is Depth + 1,
        unfold_and_check(Conjunct, Class, Depth1).


unfold_and_flatten(Depth, Terms2) :-
      unfold(Depth, Terms), flatten(Terms, Terms2). 

unfold_and_check(Conjunct, Class, Depth) :-
         findall(Terms, unfold_and_flatten(Depth, Terms), List),
         not(check_list(List, Conjunct)),
         Depth1 is Depth + 1,
         unfold_and_check(Conjunct, Class, Depth1).

check_term(Terms, Conjunct) :-
        prepare_conjunct(Conjunct, ConjunctStr),
        flatten(Terms, Terms2),
        get_terminals(Terms2, Terms3),
        prepare_terms(Terms3, TermsStr),
        append_definitions_to_theory([ConjunctStr, TermsStr]),
        solve_models(equiv, success).

check_list([H|_], Conjunct) :-
         check_term(H, Conjunct), !.

check_list([H|T], Conjunct) :-
        not(check_term(H, Conjunct)),
        check_list(T, Conjunct).


% rule(head(list), body([node(h), num(kh), num(var('KX')), edge(h, 'X'), key(h, kh), key('X', 'KX'), lt(kh, 'KX'), suffix('X')])).
% rule(head(suffix(t)), body([])).
% rule(head(suffix('X'),body([node('X'), node('Y'), num('KX'), num('KY'), key('X', 'KX'), key('Y', 'KY'), lt('KX', 'KY'), suffix('Y')])).

rule(head(suffix(t)), body([])).
rule(head(suffix(var(x))), body([node(var(x)), node(var(y)), num(var(kx)), num(var(ky)), edge(var(x), var(y)),
          key(var(x), var(kx)), key(var(y), var(ky)), lt(var(kx), var(ky)), suffix(var(y))])).

rule(head(list), 
	 body([
  node(h),
  num(kh),
  num(var(kx)),
  edge(h, var(x)),
  key(h, kh),
  key(var(x), var(kx)),
  lt(kh, var(kx)),
  suffix(var(x)),
  node(var(x))
])).


rule(head(tree), body([node(var(x)), root(var(x)), node(var(y)), left(var(x), var(y)), node(var(z)),
                       right(var(x), var(z)), tree(var(y)), tree(var(z))])).

rule(head(tree(nil)), body([])).

rule(head(tree(x)), body([node(var(x)), node(var(y)), left(var(x), var(y)), node(var(z)),
                       right(var(x), var(z)), tree(var(y)), tree(var(z))])).


terminal(edge).
terminal(node).
terminal(num).
terminal(key).
% start(list).
nonterminal(suffix, 1).
 start(tree).
nonterminal(tree, 1).

unfold(Depth, [Terms|Terms1]) :- 
     start(Start),
     find_rule(Start, Rule), 
     Rule = rule(head(Start), body(Body)),
     vars_of(Body, Vars2),
     gen_fresh_symbols(Vars2, Body, FreshVars),
     nonground_terms(Body, Nonground),
     set_diff(Body, Nonground, TermsGround),
     (Depth = 0 -> (
                   %all_possible_substitutions(FreshVars, Body, Terms),
                   purge_vars_with_constants(Nonground, TermsSubstituted),
                   append(TermsGround, TermsSubstituted, Terms),
                   Terms1 = [])
                ;
     subst_fresh_vars_conjunct(FreshVars, Body, Body1),
     get_nonterminals(Body1, NonTerminals),
     set_diff(Body1, NonTerminals, Terms),
     pair_depth(NonTerminals, 0, Remainder),
     unfold(Remainder, Terms1, Depth)
     ).


unfold([], [], _).

find_rule(Pred, Rule) :-
    rule(head(Head), body(B)),
    Pred =..[Name|Args1],
    Head =..[Name|Args2],
    length(Args1, L),
    length(Args2, L),
    Rule = rule(head(Head), body(B)).


unfold([(Pred, CurrentDepth)|T], [Terms|Terms1], Depth) :-
       CurrentDepth < Depth - 1,
       find_rule(Pred, Rule),
       Pred =.. [_|Args],
       Rule = rule(head(Head), body(Body)),
       Head =.. [_|Vars],
       pair_symbol_vars(Args, Vars, Pairs),
       subst_fresh_vars_conjunct(Pairs, Body, Body1),
       vars_of(Body1, Vars2),
       gen_fresh_symbols(Vars2, Body1, Vars3),
       subst_fresh_vars_conjunct(Vars3, Body1, Body2),
       get_nonterminals(Body2, NonTerminals),
       set_diff(Body2, NonTerminals, Terms),
       (NonTerminals \= [] -> 
              (CurrentDepth1 is CurrentDepth + 1,
              pair_depth(NonTerminals, CurrentDepth1, Continuation),
              append(T, Continuation, T1),
              unfold(T1, Terms1, Depth)) 
                           ;
              unfold(T, Terms1, Depth)).

unfold([(Pred, CurrentDepth)|T], [Terms|Terms1], Depth) :-
           CurrentDepth is Depth - 1,
             find_rule(Pred, Rule),
           Pred =.. [_|Args],
           Rule = rule(head(Head), body(Body)),
           Head =.. [_|Vars],
       pair_symbol_vars(Args, Vars, Pairs),
       subst_fresh_vars_conjunct(Pairs, Body, Body1),
       get_nonterminals(Body1, NonTerminals),
       set_diff(Body1, NonTerminals, Terms),
      (NonTerminals \= [] -> 
              (
              pair_depth(NonTerminals, Depth, Continuation),
              append(T, Continuation, T1),
              unfold(T1, Terms1, Depth)) 
                           ;
              unfold(T, Terms1, Depth)).

unfold([(Pred, Depth)|T], [Terms|Terms1], Depth) :-
      find_rule(Pred, Rule),
       Pred =.. [_|Args],
       Rule = rule(head(Head), body(Body)),
       Head =.. [_|Vars],
       pair_symbol_vars(Args, Vars, Pairs),
       subst_fresh_vars_conjunct(Pairs, Body, Body1),
       nonground_terms(Body1, Nonground),
       set_diff(Body1, Nonground, TermsGround),
       %all_possible_substitutions(Pairs, Nonground, TermsSubstituted),
       purge_vars_with_constants(Nonground, TermsSubstituted),
       append(TermsGround, TermsSubstituted, Terms),
       unfold(T, Terms1, Depth).


term_vars_2(Head, Vars) :-
     Head =..[_|Args],
     filter_vars(Args, Vars).

filter_vars([var(X)|T], [X|R]) :-
        filter_vars(T, R).

filter_vars([H|T], R) :-
          H \= var(_),
          filter_vars(T, R).

filter_vars([], []).

var_type(Var, [H|_], Domain) :-
        H =.. [Domain, var(Var)], 
        domain(Domain), !.

var_type(Var, [H|T], Domain) :-
        H =.. [D, var(Var)], 
        not(domain(D)), 
        var_type(Var, T, Domain).	

var_type(Var, [H|T], Domain) :-
        H =..[X, var(V)],
        Var \= V,
        var_type(Var, T, Domain).

var_type(Var, [H|T], Domain) :-
        H =.. [_, V],
        V \= var(_),
        var_type(Var, T, Domain).

var_type(Var, [H|T], Domain) :-
        H =.. [_|Args],
        length(Args, L), 
        L > 1,
        var_type(Var, T, Domain).

gen_fresh_symbol(Var, Body, (Var, FreshVar)) :-
         var_type(Var, Body, Domain),
         generate_symbol(Domain, FreshVar).

gen_fresh_symbols([H|T], Body, [Tuple|R]) :- 
         gen_fresh_symbol(H, Body, Tuple),
         gen_fresh_symbols(T, Body, R).

gen_fresh_symbols([], _, []).


substitute_fresh_var((Old, New), Term, Term2) :-
		Term =.. [Name|Args],		
        replace(var(Old), New, Args, Args1),
        Term2 =.. [Name|Args1].

substitute_fresh_vars([H|T], Term, Term2) :-
        substitute_fresh_var(H, Term, Term1),
        substitute_fresh_vars(T, Term1, Term2).

substitute_fresh_vars([], Term, Term).
    
	
subst_fresh_vars_conjunct(Vars, [H|T], [H1|R]) :-
         substitute_fresh_vars(Vars, H, H1),
         subst_fresh_vars_conjunct(Vars, T, R).

subst_fresh_vars_conjunct(_, [], []).


get_nonterminals([H|T], [H|R]) :-
      H =..[Name|Args],
      length(Args, L),
      nonterminal(Name, L), !, 
      get_nonterminals(T, R).

get_nonterminals([H|T], R) :-
      H =..[Name|Args],
      length([Name|Args], L),
      not(nonterminal(Name, L)), !, 
      get_nonterminals(T, R).

get_nonterminals([], []).


pair_symbol_vars([H|T], [var(X)|T1], [(X,H)|R]) :-
          pair_symbol_vars(T, T1, R).

pair_symbol_vars([H|T], [H1|T1], R) :-
         H1 \= var(_),
         pair_symbol_vars(T, T1, R).

pair_symbol_vars([], [], []).


has_substitutable_vars([H|_]) :-
         H =.. [Domain, var(_)],
         constants(_, Domain).          

has_substitutable_vars([H|T]) :-
        H =.. [Domain, var(_)],
        not(constants(_, Domain)),
        has_substitutable_vars(T).

has_substitutable_vars([H|T]) :-
        H =.. [Name, X],
        X \== var(_),
        has_substitutable_vars(T).

% not doing type inference, assumed that all types are known via domain predicates
has_substitutable_vars([H|T]) :-
        H =.. List,
        length(List, L),
        L > 2,
        has_substitutable_vars(T).



subst_fresh_symbol_conjunct(Symbol, Var, [H|T], [H1|R]) :-
          substitute_fresh_var((Var, Symbol), H, H1),
          subst_fresh_symbol_conjunct(Symbol, Var, T, R).

subst_fresh_symbol_conjunct(_, _, [], []).


purge_vars_with_constants(Conjunct, Conjunct) :- 
          not(has_substitutable_vars(Conjunct)).


purge_vars_with_constants(Conjunct, Conjunct2) :-
         has_substitutable_vars(Conjunct),
         subst_constants(Conjunct, Conjunct1),
         purge_vars_with_constants(Conjunct1, Conjunct2).



%subst_constants(Conjunct, Conjunct1) :-
%        maplist(term_vars_2, Conjunct, Vars),
%        flatten(Vars, Vars1),
%        remove_duplicates(Vars1, Vars2),
%        member(V, Vars2),
%        var_type(V, Conjunct, Domain),
%        constants(C, Domain),
%         subst_fresh_symbol_conjunct(C, V, Conjunct, Conjunct1).

subst_constants(Conjunct, Conjunct1) :-
        maplist(term_vars_2, Conjunct, Vars),
        flatten(Vars, Vars1),
        remove_duplicates(Vars1, Vars2),
        member(V, Vars2),
        var_type(V, Conjunct, node),
        terminal_node_symbol(C),
        subst_fresh_symbol_conjunct(C, V, Conjunct, Conjunct1).

subst_constants(Conjunct, Conjunct1) :-
        maplist(term_vars_2, Conjunct, Vars),
        flatten(Vars, Vars1),
        remove_duplicates(Vars1, Vars2),
        member(V, Vars2),
        var_type(V, Conjunct, Domain),
        Domain \== node.
        % terminal_node_symbol(C),
        % subst_fresh_symbol_conjunct(C, V, Conjunct, Conjunct1).



pair_depth([H|T], Depth, [(H,Depth)|R]) :- 
         pair_depth(T, Depth, R).

pair_depth([], _, []).



all_possible_substitutions(_, NonTerminals, Terms) :-
        purge_vars_with_constants(NonTerminals, Terms).

all_possible_substitutions([H|T], NonTerminals, Terms) :-
      H = (A, B),
      subst_fresh_symbol_conjunct(B, A, NonTerminals, Terms1),
      all_possible_substitutions(T, Terms1, Terms).

all_possible_substitutions([_|T], NonTerminals, Terms) :-
            all_possible_substitutions(T, NonTerminals, Terms).


all_possible_substitutions([], _, []).


vars_of(Conjunct, Vars2) :-
     maplist(term_vars_2, Conjunct, Vars),
     flatten(Vars, Vars1),
     remove_duplicates(Vars1, Vars2).
     
nonground_terms([H|T], [H|R]) :-
      H =.. [_|Args],
      member(var(_), Args),
      nonground_terms(T, R).

nonground_terms([H|T], R) :-
      H =.. [_|Args],
      not(member(var(_), Args)),
      nonground_terms(T, R).


nonground_terms([], []).


prepare_conjunct(Conjunct, Str4) :-
        % conjunct_str(Conjunct, BodyStr),
        maplist(format_conjunct_term, Conjunct, BodyStr),
        list_str(BodyStr, BodyStr1),
        string_concat('condition :- ', BodyStr1, Str),
        string_concat(Str, '.\n', Str2),
        string_concat(Str2, '\n:- not condition.\n', Str3),
        lint_string(Str3, Str4).

prepare_terms([H|T], Str2) :-
       % format_conjunct_term(H, S1),
       term_string(H, S1),
       string_concat(S1, '.\n', S2),
       prepare_terms(T, S3),
       string_concat(S2, S3, Str),
       lint_string(Str, Str2).

prepare_terms([], '').  

test([node(x), node(y), reachable(x), edge(x, y), suffix(y), num(kx), num(ky), key(x, kx), key(y, ky), node(target), num(ktarget), key(target, ktarget), lt(kx, ktarget), lt(ktarget, ky), not(reachable(target))]).
test2([node(x), node(y), reachable(x), edge(x, target), edge(target, y), suffix(y), num(kx), num(ky), key(x, kx), key(y, ky), node(target), num(ktarget), key(target, ktarget), lt(kx, ktarget), lt(ktarget, ky)]).
get_terminals(List, Terminals) :- 
       get_nonterminals(List, NonTerminals),
       set_diff(List, NonTerminals, Terminals).