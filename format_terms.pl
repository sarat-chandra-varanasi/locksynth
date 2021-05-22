% :- module(format_terms, []).

time_dependent2(Name) :-
      signature(Name, _, Domains),
      member(time, Domains).

add_time_list(TimeSymbol, [H|T], [not(Term2)|R]) :-
       H = (not(Term)),
       Term =.. [Name|Suffix],
       time_dependent(Name, _),
       append([Name|Suffix], [TimeSymbol], List2),
       Term2 =.. List2,
       add_time_list(TimeSymbol, T, R).

add_time_list(TimeSymbol, [H|T], [Term|R]) :-
       H \= (not(_)),
       H =.. [Name|Suffix],
       time_dependent(Name, _),
       append([Name|Suffix], [TimeSymbol], List2),
       Term =.. List2,
       add_time_list(TimeSymbol, T, R).

add_time_list(TimeSymbol, [H|T], [H|R]) :-
       H = (not(Term)),
       Term =.. [Name|Suffix],
       not(time_dependent(Name, _)),
       add_time_list(TimeSymbol, T, R).

add_time_list(TimeSymbol, [H|T], [H|R]) :-
       H \= (not(_)),
       H =.. [Name|Suffix],
       not(time_dependent(Name, _)),
       add_time_list(TimeSymbol, T, R).


add_time_list(_, [], []).



add_time_list_constant(TimeSymbol, [H|T], [not(Term2)|R]) :-
       H = (not(Term)),
       Term =.. [Name|Suffix],
       time_dependent(Name, _),
       append([Name|Suffix], [constant(TimeSymbol)], List2),
       Term2 =.. List2,
       add_time_list_constant(TimeSymbol, T, R).

add_time_list_constant(TimeSymbol, [H|T], [Term|R]) :-
       H \= (not(_)),
       H =.. [Name|Suffix],
       time_dependent(Name, _),
       append([Name|Suffix], [constant(TimeSymbol)], List2),
       Term =.. List2,
       add_time_list_constant(TimeSymbol, T, R).

add_time_list_constant(TimeSymbol, [H|T], [H|R]) :-
       H = (not(Term)),
       Term =.. [Name|Suffix],
       not(time_dependent(Name, _)),
       add_time_list_constant(TimeSymbol, T, R).

add_time_list_constant(TimeSymbol, [H|T], [H|R]) :-
       H \= (not(_)),
       H =.. [Name|Suffix],
       not(time_dependent(Name, _)),
       add_time_list_constant(TimeSymbol, T, R).


add_time_list_constant(_, [], []).



add_time(Term, Term2) :-
    lint_negation(Term, Term1),
    Term1 =.. [Name|_],
    time_dependent2(Name),
    Term1 =.. List,
    generate_symbol(time, Symbol),
    append(List,[Symbol], List2),
    Term2 =.. List2.

add_time(Term, Term) :-
    lint_negation(Term, Term1),
    Term1 =.. [Name|_],
    not(time_dependent2(Name)).


add_time_unconditional(TimeSymbol, Term, Term2) :-
          lint_negation(Term, Term1),
          Term1 =.. List,
          append(List, [TimeSymbol], List2),
          Term2 =.. List2.
 
add_time_unconditional_constant(TimeSymbol, Term, Term2) :-
          lint_negation(Term, Term1),
          Term1 =.. List,
          append(List, [constant(TimeSymbol)], List2),
          Term2 =.. List2.


map_vars_domain([H|T], [D|Tail], [Term|R]) :- 
         Term =.. [D,H],
         map_vars_domain(T, Tail, R).

map_vars_domain([], _, []).

term_domainvars(Term, DomainVars) :-
      lint_negation(Term, Term1),
      Term1 =.. [Name|Vars],
      get_domains(Term1, Domains),
      map_vars_domain(Vars, Domains, DomainVars).

map_vars_domain([H|T], [D|Tail], [Term|R]) :- 
         [H|T] \= [],
         Term =.. [D,H], 
         map_vars_domain(T, Tail, R), !.

map_vars_domain([], _, []).

tuple_1([(H,_)|T], [H|R]) :-
               tuple_1(T, R).
tuple_1([], []).

term_vars(Term, Vars) :-
       lint_negation(Term, Term2),
       Term2 =.. [_|Vars].

conjunct_vars(List, Vars) :-
    maplist(term_vars, List, VarsMap),
    flatten(VarsMap, Flattened),
    remove_duplicates(Flattened, Vars).

neg_term(Term, NegTerm) :-
      term_string(Term, String),
      string_concat('neg_',String, NegString),
      term_string(NegTerm, NegString).

conjunct_domainvars(List, DomainVars) :-
     maplist(term_domainvars, List, DomainVarsMap),
     flatten(DomainVarsMap, Flattened),
     remove_duplicates(Flattened, DomainVars).


lint_negation(Term, Term2) :-
      Term =.. [not, Term2], !.

lint_negation(Term, Term).


time_var(Term1, Var) :-
      Term1 =.. List,
      last_element(List, Var).

copy_timevar(Term1, Term2, Term3) :-
       time_var(Term1, Var),
       append_var(Var, Term2, Term3).

last_element(List, X) :-
      append(_, [X], List).

append_var(Var, Term, Term1) :-
        Term =.. List,
        append(List, [Var], List1),
        Term1 =.. List1.


get_domains(Term, Domains) :-
      Term =.. [Name|_],
      signature(Name, _, Domains).

domain_variant(Domain, Variant) :-
       Domain =.. [Name,_],
       generate_symbol(Name, Symbol),
       Variant =.. [Name,Symbol].


var_domain(Domain, Var) :-
        Domain =.. [_,Var].

rename_vars(Term, Domains, Term2) :-
    Term =.. [Name|_],
    maplist(var_domain, Domains, List),
    Term2 =.. [Name|List].

% Term2 borrows the same timestep from Term1
same_timevar(Term1, Term2, Term3) :-
      Term1 =.. List,
      last_element(List, TimeVar),
      Term2 =.. List2,
      append(List3, [_], List2),
      append(List3, [TimeVar], List4),
      Term3 =.. List4.

replace_same_vars(Head1, Domains1, Head2, Domains2, Head3, Domains3) :-
        common_vars(Head1, Head2, Common),
        rename_common_vars(Common, Head2, Domains2, Head3, Domains3).


rename_common_vars(CommonVars, Term, Domains, Term2, Domains2) :-
      Term =.. [Name|Vars],
      rename_common_vars_helper(CommonVars, Domains, Vars, Vars2, Domains2),
      Term2 =.. [Name|Vars2].

rename_common_vars_helper([H|T], Domains, Vars, Vars2, Domains2) :-
      domain_of(H, Domains, Domain),
      generate_symbol(Domain, FreshVar),
      replace(H, FreshVar, Vars, Vars1),
      domain_term_of(H, Domains, Term),
      FreshTerm =.. [Domain,FreshVar],
      replace(Term, FreshTerm, Domains, Domains1),
      rename_common_vars_helper(T, Domains1, Vars1, Vars2, Domains2).

rename_common_vars_helper([], Domains, Vars, Vars, Domains).    

domain_term_of(Var, [H|T], H) :-
      H =.. [_, Var].

domain_term_of(Var, [H|T], Term) :-
      H =.. [_, V],
      Var \== V,
      domain_term_of(Var, T, Term).

domain_of(Var, [H|T], Domain) :-
        H =.. [Domain, Var].

domain_of(Var, [H|T], Domain) :-
        H =.. [D,V],
        V \== Var,
        domain_of(Var, T, Domain).


common_vars(Term1, Term2, Common) :-
        Term1 =.. [_|Vars1],
        Term2 =.. [_|Vars2],
        set_intersect(Vars1, Vars2, Common).


format_term(Term) :-
      Term = [Name|Args].


format_conjunct_term(Term, Formatted) :- 
      Term =.. [diff_term, X, Y], !,
      capitalize(X, X1),
      capitalize(Y, Y1),
      term_string(X1, X1s),
      term_string(Y1, Y1s),
      string_concat(X1s, " != ", X1s1),
      string_concat(X1s1, Y1s, Formatted).


format_conjunct_term(Term, Formatted) :-
       Term =.. [not, Term2],
       format_conjunct_term(Term2, Format2),
       string_concat('not ', Format2, Formatted).

format_conjunct_term(Term, Formatted) :-
       Term =.. [Name|Args],
       Name \== not,
       maplist(capitalize, Args, Caps),
       list_str(Caps, ArgsStr),
       term_string(Name, NameStr),
       string_concat(NameStr, '(',  S1),
       string_concat(S1, ArgsStr, S2),
       string_concat(S2, ')', Formatted).

format_conjunct_term(Term, Formatted) :-
       Term =.. [Name],
       term_string(Name, Formatted).


capitalize(constant(Symbol), Symbol).

capitalize(Symbol, Str) :-
       constants(Symbol, _),
       term_string(Symbol, Str).

capitalize(Symbol, Caps) :-
       not(constants(Symbol, _)),
       term_string(Symbol, Str),
       string_chars(Str, [H|T]),
       char_code(H, Code),
       CapsCode is Code - 32,
       char_code(H1, CapsCode),
       string_chars(Caps, [H1|T]).


dom_of(Arg, Body, Domain) :-
       is_var(Arg, Body),
       domain_of(var(Arg), Body, Domain).


dom_of(Arg, Body, Domain) :-
      not(is_var(Arg, Body)),
      constants(Arg, Domain).


is_var(Arg, [Term|_]) :-
         Term =.. [_, var(Arg)].

is_var(Arg, [X|T]) :-
      X =.. [_|Args],
      length(Args, L), 
      L > 1,
      is_var(Arg, T).

is_var(Arg, [X|T]) :- 
      X =.. [_, A],
      A \= Arg,
      A \= var(Arg),
      is_var(Arg, T).


to_var(Term, Term1) :-
      Term \= not(_),
      Term =.. [Name|Args],
      maplist(to_var_helper, Args, Args1),
      Term1 =.. [Name|Args1].

to_var(Term, not(Term2)) :-
      Term = not(Term1),
      Term1 =.. [Name|Args],
      maplist(to_var_helper, Args, Args1),
      Term2 =.. [Name|Args1].
            
to_var_helper(X, var(X)) :-
          not(constants(X, _)).
to_var_helper(X, X) :-
         constants(X, _).