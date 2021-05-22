
gen_falsify_pred(Pred, R) :- 
      signature(Pred, _, Domains),
      maplist(generate_symbol, Domains, Symbols),
      term_string(Pred, Predstr),
      string_concat(Predstr, "_falsify", Headstr),
      term_string(Headname, Headstr),
      Positive =.. [Pred|Symbols],
      last_element(Symbols, Time),
      generate_symbol(time, NextTime),
      replace(Time, NextTime, Symbols, Symbols1),
      Positive1 =.. [Pred|Symbols1],
      Naf =.. [not, Positive1],
      Body = [Positive, next_time(Time, NextTime), Naf],
      R = rule(head(Headname), body(Body)).


 gen_falsify_theory(Rules) :-
       findall(X, time_dependent(X, _), Preds),
       maplist(gen_falsify_pred, Preds, Rules).


check_falsify(Pred, Success) :-
      reset_counts, 
      max_depth(Length),
      time_chain(Length, Chain),
      gen_falsify_pred(Pred, Rule),
      gen_interference_theory(T),
      Rule = rule(head(Rulename), _),
      append(T, [Rule, constraint([not(Rulename)])], F),
      append(F, Chain, F1),
      write_rules(F1, S),
      rules_str(S, S1),
      string_concat('falsify_', Pred, CheckFalsify),
      add_to_base_theory(CheckFalsify, S1),
      solve_models(CheckFalsify, one, Models),
      (Models \= [] -> Success = true ; Success = false).

