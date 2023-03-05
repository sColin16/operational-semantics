include Types
include Print_utils

(** [tree_valid alphabet tree] tests whether the provided tree is a valid tree over the provided alphabet *)
let rec tree_valid (alpha: ranked_alphabet) (tree_inst: tree) =
  let Node (symbol, args) = tree_inst in
      match (SymbolMap.find_opt symbol alpha) with
      | None -> false
      | Some(arity) ->
          (List.length args) = arity &&
          List.for_all (tree_valid alpha) args

(** [sent_tree_valid non_terms alphabet sent_tree] tests whether the provided sentential tree is valid over the provided alphabet and non-terminal set*)
let rec sent_tree_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (sent_tree: sent_tree) =
    match sent_tree with
    | Leaf(non_term) -> NonTerminalSet.mem non_term non_terms
    | Node(symbols, args) -> (match (SymbolMap.find_opt symbols alpha) with
        | None -> false
        | Some(arity) ->
            (List.length args) = arity &&
            List.for_all (sent_tree_valid non_terms alpha) args)

(** [production_valid non_terms alphabet non_term tree_set] tests whether the
     production provided as the pair of [non_term] and [tree_set] is valid over the
     given alphabet and non-terminal set *)
let production_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (non_term: non_terminal) (tree_set: SententialTreeSet.t) =
    NonTerminalSet.mem non_term non_terms &&
        SententialTreeSet.for_all (sent_tree_valid non_terms alpha) tree_set

let productions_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (productions: tree_grammar_productions) =
    NonTerminalMap.for_all (production_valid non_terms alpha) productions

let grammar_valid (grammar: regular_tree_grammar) =
    NonTerminalSet.mem grammar.start_non_terminal grammar.non_terminals &&
    productions_valid grammar.non_terminals grammar.ranked_alphabet grammar.productions

let non_term_label non_term =
    let NonTerminal(label) = non_term in label

let symbol_label symbol =
    let Symbol(label) = symbol in label
