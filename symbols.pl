

:- dynamic symbol_count/2.

init_counts :- 
     findall(D, domain(D), List),
     init_counts(List).

init_counts([H|T]) :-
     assert(symbol_count(H, 0)),
     init_counts(T).

init_counts([]).


reset_counts :- 
      findall(D, domain(D), List),
      reset_counts(List).

reset_counts(List) :-
       reset_counts_helper(List),
       init_counts(List).

reset_counts_helper([H|T]) :-
       retractall(symbol_count(H, _)),
       reset_counts_helper(T).

reset_counts_helper([]).

generate_symbol(Domain, Symbol) :-
     symbol_prefix(Domain, Prefix),
     symbol_count(Domain, Count),
     Count1 is Count + 1,
     term_string(Prefix, PrefixString),
     term_string(Count1, CountString),
     string_concat(PrefixString, CountString, SymbolString),
     term_string(Symbol, SymbolString),
     retractall(symbol_count(Domain, _)),
     assert(symbol_count(Domain, Count1)).

