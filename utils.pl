

flatten([H|T], [H|R]) :-
      H \= [],
      H \= [_|_], !,
      flatten(T, R).

flatten([H|T], R) :-
      H = [], !,
      flatten(T, R).

flatten([H|T], R) :-
      !,
      flatten(H, R1),
      flatten(T, R2),
      append(R1, R2, R).

flatten([], []).


remove_duplicates([H|T],R) :-
         member(H, T), !,
         remove_duplicates(T, R).

remove_duplicates([H|T], [H|R]) :-
          not(member(H, T)), !,
          remove_duplicates(T, R).
    
remove_duplicates([], []).

write_to_file(File, Content) :-
      current_output(PrevStream),
      open(File, write, NewStream),
      set_output(NewStream),
      write(Content),
      set_output(PrevStream),
      close(NewStream).


set_intersect([H|T], List, [H|R]) :-
           member(H, List),
           set_intersect(T, List, R).

set_intersect([H|T], List, R) :-
           not(member(H, List)),
           set_intersect(T, List, R).

set_intersect([], _, []).


replace(X, Y, [X|T], [Y|R]) :-
             replace(X, Y, T, R).

replace(X, Y, [X1|T], [X1|R]) :-
              X \= X1,
              replace(X, Y, T, R).

replace(X, Y, [], []).


 maplist_general(Pred, [H|T], [Result|R]) :-
         append(H, [Result], H1),
         Term =.. [Pred|H1],
         call(Term),
         maplist_general(Pred, T, R).

maplist_general(_, [], []).


set_diff([H|T], List, [H|R]) :-
           not(member(H, List)),
           set_diff(T, List, R).


set_diff([H|T], List, R) :-
          member(H, List),
          set_diff(T, List, R).


set_diff([], _, []).


pair([H|T], [H1|T1], [(H, H1)|R]) :-
             pair(T, T1, R).

pair([], [], []).

conjunct_str([], '').

conjunct_str([H], Str) :-
      term_string(H, Str).


conjunct_str([H|T], Str) :-
      term_string(H, S1),
      string_concat(S1, ',', S2),
      conjunct_str(T, S3),
      string_concat(S2, S3, Str).

list_str([H], H).

list_str([H|T], Str) :-
      string_concat(H, ', ', S2),
      conjunct_str(T, S3),
      string_concat(S2, S3, Str).

lint_string(Str, Str2) :-
      string_chars(Str, List),
      remove('"', List, List1),
      string_chars(Str2, List1).


remove(H, [H|T], R) :-
         remove(H, T, R).

remove(H, [X|T], [X|R]) :-
         X \== H, 
         remove(H, T, R).

remove(_, [], []).

rules_str(Rules, Str) :-
     rules_str_helper(Rules, "", Str).

rules_str_helper([H|T], S, S2) :-
         string_concat(H, S, S1),
         rules_str_helper(T, S1, S2).

rules_str_helper([], S, S).


pair(X, [H|T], [(X,H)|R]) :-
         pair(X, T, R).

pair(_, [], []).


choose_2(List, Combinations) :-
      choose_2_helper(List, Combos),
      flatten(Combos, Combinations).

choose_2_helper([H|T], [R|R1])  :-
      pair(H, T, R),
      choose_2_helper(T, R1).

choose_2_helper([], []).


pair_list_value_helper([H|T], Value, [(H, Value)|R]) :-
         pair_list_value_helper(T, Value, R).

pair_list_value_helper([], _, []).

pair_list_value(List, Value, Pairs) :-
        pair_list_value_helper(List, Value, Pairs2),
        flatten(Pairs2, Pairs).

clear :- 
     shell('clear', _).

b :-
     shell('./merge.sh', _).


build :-
     shell('./merge.sh', _).

set_ds(Val) :-
      retractall(ds(_)),
      assert(ds(Val)).


add_underscore(A, B, C) :-
    string_concat(A, "_", As),
    string_concat(As, B, C).

reduce(List, Op, I, X) :-
      List = [A, B|T],
      call(Op, A, B, X1),
      reduce_helper(T, Op, X1, X).

reduce_helper([], Op, P, P).          

reduce_helper([H|T], Op, P, X) :-
         call(Op, P, H, X1),
         reduce_helper(T, Op, X1, X).  
          
sum(X, Y, Z) :- Z is X + Y. 

l :- [merge].
bl :- b, l.

exit :- halt.

zip(X, [H|T], [(X,H)|R]) :-
      !, zip(X, T, R).

zip(_, [], []). 