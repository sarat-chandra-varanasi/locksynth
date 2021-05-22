

check_rcu :- 
  detect_key_movement(X),
  X = true.

check_rcu :-
  ds(DS),
  code(DS, Op, _, Block, _, _, _),
  not(check_reorder(Op, Block)).

program_orders(Orders) :-
    ds(DS),
    findall((Op,Block), code(DS, Op, _, Block, _, _, _), List),
    get_valid_program_orders(List, Orders).

get_valid_program_orders([H|T], [(Op,Block,Order)|Orders]) :-
      H = (Op, Block),
      check_reorder(Op, Block, Order),
      get_valid_program_orders(T, Orders).

get_valid_program_orders([], []).

lock_stmts([H|T], [lock(H)|R]) :-
      lock_stmts(T, R).

lock_stmts([], []).

unlock_stmts([H|T], [unlock(H)|R]) :-
      unlock_stmts(T, R).

unlock_stmts([], []).

validate_cond(Pre, Steps, (if(Pre), Steps)).

gen_concurrent_code(Op, Block, Code2) :-
    check_reorder(Op, Block, Order), !, 
    ds(DS),
    code(DS, Op, _, Block, Pre, _, _),
    get_nodes(Pre, Nodes),
    lock_stmts(Nodes, Locks),
    unlock_stmts(Nodes, Unlocks),
    validate_cond(Pre, Order, CondCode),
    append(Locks, [CondCode], Code),
    append(Code, Unlocks, Code2).

whitespace(Intent, Str) :-
     whitespace(Intent, '', Str).

whitespace(0, P, P).

whitespace(Indent, P, S) :-
       Indent > 0,
       Indent1 is Indent - 1,
       string_concat(P, ' ', P1),
       whitespace(Indent1, P1, S).

gen_text(Code, Text) :- 
    gen_text(Code, 0, '', Text).

gen_text([], _, P, P).

gen_text([Step|T], Indent, P, Text) :-
       Step \= (if(_),_),
       whitespace(Indent, W),
       term_string(Step, S),
       string_concat(W, S, P1),
       string_concat(P1, '\n', P2),
       string_concat(P, P2, P3),
       gen_text(T, Indent, P3, Text).

to_text(if(Pre), S1) :-
        conjunct_text(Pre, Pres),
        string_concat('if validate(', Pres, S),
        string_concat(S, '){\n', S1).

conjunct_text([H], S) :-
        term_string(H, S).

conjunct_text([H|T], S) :-
       term_string(H, Hs), 
       string_concat(Hs, ' & ', Hs1),
       conjunct_text(T, Ts),
       string_concat(Hs1, Ts, S).

gen_text([(if(Pre), Steps)|T], Indent, P, Text) :-
       to_text(if(Pre), S1),
       Indent1 is Indent + 2,
       gen_text(Steps, Indent1, '', Text1),
       string_concat(S1, Text1, Text2),
       string_concat(Text2, '\n}\n', Text3),
       gen_text(T, Indent, Text3, Text4),
       string_concat(P, Text4, Text).


synth :- 
   current_prolog_flag(argv, Argv),
   Argv = [_, DS1],
   term_string(DS, DS1),
   set_ds(DS),
   op_blocks(OpBlocks),
   detect_key_movement(false),
   synth(OpBlocks), 
   exit.

synth([(Op,Block)|T]) :-
    check_adequacy(Op, Block, true),
    write(Op), 
    write('::'),
    write(Block),
    write('{\n'),
    gen_concurrent_code(Op, Block, Code),
    gen_text(Code, Text),
    write(Text), 
    write('\n}\n'),
    synth(T).

synth([]).