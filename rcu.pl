

generate_traversal_theory(Theory2) :-
    reset_counts, 
     max_depth(Length), 
     time_chain(Length, Chain),
     TimeTransitiveClosureRules = 
       [rule(head(greater(t1, t2)), body([time(t1), time(t2), next_time(t2, t1)])),
        rule(head(greater(t1, t2)), body([time(t1), time(t2), time(t3), greater(t1, t3), greater(t3, t2)]))
      ],
      ds(DS),
     start_node(DS, StartNode),
     Start = rule(head(visited(StartNode, t1)),body([time(t1)])),
     end_node(DS, EndNode),
     EndNodeFact = rule(head(end_node(constant(EndNode))), body([])), 
     Transitive = rule(head(visited(y, t2)), body([time(t1), time(t2), next_time(t1, t2), visited(x, t1), next_node(target, x, y, t1)])),
     TraversalDone = rule(head(done(t1)), body([time(t1), visited(x, t1), end_node(x)])),
     TraversalDoneDominated = rule(head(dominating(t1)),body([time(t1), time(t2), done(t1), done(t2), greater(t1, t2)])),
     EarliestTimeTraversalDone = rule(head(earliest(t1)), body([time(t1), done(t1), not(dominating(t1))])),
     next_node_rules(NextNodeRules),
     StartOracle = rule(head(visitedOracle(StartNode, t1)),body([time(t1)])),
     TransitiveOracle = rule(head(visitedOracle(y, t1)), body([time(t1), visitedOracle(x, t1), next_node(target, x, y, t1)])),  
     append(Chain, NextNodeRules, NextNodeAndChain),
     Theory = [Start, Transitive, StartOracle, TransitiveOracle, TraversalDone, TraversalDoneDominated,
               EarliestTimeTraversalDone, EndNodeFact | NextNodeAndChain], 
     append(Theory, TimeTransitiveClosureRules, Theory2).


next_node_rules(Rules) :-
     ds(DS),
     findall((CurrentNode, NextNode, Cond), next_node(DS, CurrentNode, (NextNode, Cond)), List),
     maplist(next_node_rules_helper, List, Rules).

next_node_rules_helper((CurrentNode, NextNode, Cond), Rule) :-
     add_time_list(t1, Cond, Cond1),
     Rule = rule(head(next_node(target, CurrentNode, NextNode, t1)), body([time(t1)|Cond1])).
     
time_chain(Length, Chain) :-
	   Length > 0,
       time_chain(0, Length, Chain).

time_chain(0, Length, Chain) :-
     generate_symbol(time, Time),
     Rule = rule(head(time(constant(Time))), body([])),
     time_chain(1, Length, Time, [Rule], Chain).



time_chain(N, Length, PrevTime, P, Chain) :-
      N >= 1,
      N < Length,
      generate_symbol(time, Time),
      N1 is N + 1, 
      Rule1 = rule(head(time(constant(Time))), body([])),
      Rule2 = rule(head(next_time(constant(PrevTime), constant(Time))),body([])),
      append(P, [Rule1, Rule2], P1), !, 
      time_chain(N1, Length, Time, P1, Chain).


time_chain(Length, Length, _, Chain, Chain) :- !.

key_movement_rule(Rule) :-
      Head = key_move,  
      Body = [time(t1), earliest(t1), visitedOracle(x, t1), not(visited(x, t1))],
      Rule = rule(head(Head), body(Body)).

key_movement_constraint(constraint([not(key_move)])).

detect_key_movement(Yes) :- 
   generate_traversal_theory(T1),
   gen_interference_theory(T2),
   key_movement_rule(Rule), 
   key_movement_constraint(Constraint),
   append(T1, T2, T3),
   append(T3, [Rule, Constraint], T4),
   write_rules(T4, S),
   rules_str(S, S1),
   add_to_base_theory('rcucheck', S1), 
   solve_models('rcucheck', one, Models),
   (Models \= [] -> Yes = true ; Yes = false).
