

append_definitions_to_theory(Definitions) :-
		 shell('cat empty > theory_appended.lp', _),
         % shell('cp theory_eq.lp theory_appended.lp', _),
         shell('cp tree.lp theory_appended.lp', _),
         open('theory_appended.lp', append, Stream),
         current_output(Out),
         set_output(Stream),
         append_helper(Definitions),
         set_output(Out),
         close(Stream).

append_helper([H|T]) :-
        write(H),
        append_helper(T).

append_helper([]).


write_rule(Rule, Str2) :-
   Rule = rule(head(Head), body(Body)),
   format_conjunct_term(Head, Headfmt),
   maplist(format_conjunct_term, Body, Bodyfmt),
   term_string(Headfmt, Headstr),
   conjunct_str(Bodyfmt, Bodystr), 
   (Body \= [] ->  string_concat(Headstr, " :- ", Head1) ; 
                   Head1 = Headstr
   ),
   string_concat(Bodystr, ".\n", Body1),
   string_concat(Head1, Body1, Str),
   lint_string(Str, Str2).
   %write(Str2).


write_rule(Rule, Str2) :-
   Rule = constraint(Body),
   maplist(format_conjunct_term, Body, Bodyfmt),
   conjunct_str(Bodyfmt, Bodystr),
   string_concat(":- ", Bodystr, Bodystr1),
   string_concat(Bodystr1, ".\n", Str),
   lint_string(Str, Str2).
   %write(Str2).


write_rules([H|T], [S|S1]) :-
    write_rule(H, S), !, 
    write_rules(T, S1).

write_rules([], []) :- !.


add_to_base_theory(Extension, Rules) :-
         class_ebst(X),
         facts_rules(X, R),
         write_rules(R, R1),
         rules_str(R1, Rs),
         string_concat(Rules, Rs, Rs1),
         ds(DS),
         term_string(DS, DS1),
         string_concat(DS1, '.lp ', DSFile),
         string_concat('cp ', DSFile, CopyCmd),
         string_concat(CopyCmd, Extension, Cmd),
         % string_concat('cp list.lp ', Extension, Cmd),
         % string_concat('cp tree.lp ', Extension, Cmd),
         shell(Cmd, _),
         open(Extension, append, Stream),
         current_output(Out),
         set_output(Stream),
         write(Rs1),
         set_output(Out),
         close(Stream).


facts_rules(List, Rules) :- 
      maplist(fact_rule, List, Rules).


symbol_constant(X, constant(X)).

fact_rule(Term, Rule) :-
      Term =.. [Name|Args],
      maplist(symbol_constant, Args, Args1),
      Term1 =.. [Name| Args1],
      Rule = rule(head(Term1), body([])).

