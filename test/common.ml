open Regular_tree_language

let non_terms = NonTerminalSet.of_list [
  NonTerminal("a");
  NonTerminal("b");
  NonTerminal("c")
]

let alpha = SymbolMap.of_seq (List.to_seq [
  (Symbol("if"), 3);
  (Symbol("and"), 2);
  (Symbol("or"), 2);
  (Symbol("not"), 1);
  (Symbol("true"), 0);
  (Symbol("false"), 0)
])