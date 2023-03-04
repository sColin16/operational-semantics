(** TODO: can these just be polymorphic types that can accept anything, not just strings?
 * This would require everything to become polymoprhic, which I don't know if that can be done with the sets and maps
 * But we can make our own module for RegularTrees just like Sets and then pass the parametric type through!
 *)
type symbol = Symbol of string
type non_terminal = NonTerminal of string

module NonTerminalSet = Set.Make(
    struct
        type t = non_terminal
        let compare = compare
    end )

module NonTerminalMap = Map.Make(
    struct
        type t = non_terminal
        let compare = compare
    end )

module SymbolMap = Map.Make(
    struct
        type t = symbol
        let compare = compare
    end )

(** Map the alphabet symbols to their arity *)
type ranked_alphabet = int SymbolMap.t

type tree = Node of symbol * tree list

type sentential_tree =
    | Node of symbol * sentential_tree list
    | Leaf of non_terminal

module SententialTreeSet = Set.Make(
    struct
        type t = sentential_tree
        let compare = compare
    end )

(** Map non-terminals to a set of sential trees *)
type tree_grammar_productions = SententialTreeSet.t NonTerminalMap.t

type regular_tree_grammar = {
    non_terminals: NonTerminalSet.t;
    ranked_alphabet: ranked_alphabet;
    start_non_terminal: non_terminal;
    productions: tree_grammar_productions;
}

let rec tree_valid (alpha: ranked_alphabet) (tree_inst: tree) =
    let Node (symbol, args) = tree_inst in
        match (SymbolMap.find_opt symbol alpha) with
        | None -> false
        | Some(arity) ->
            (List.length args) = arity &&
            List.for_all (tree_valid alpha) args

let rec sent_tree_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (sent_tree: sentential_tree) =
    match sent_tree with
    | Leaf(non_term) -> NonTerminalSet.mem non_term non_terms
    | Node(symbols, args) -> (match (SymbolMap.find_opt symbols alpha) with
        | None -> false
        | Some(arity) ->
            (List.length args) = arity &&
            List.for_all (sent_tree_valid non_terms alpha) args)

let production_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (non_term: non_terminal) (tree_set: SententialTreeSet.t) =
    if NonTerminalSet.mem non_term non_terms then
        SententialTreeSet.for_all (sent_tree_valid non_terms alpha) tree_set
    else false

let productions_valid (non_terms: NonTerminalSet.t) (alpha: ranked_alphabet) (productions: tree_grammar_productions) =
    NonTerminalMap.for_all (production_valid non_terms alpha) productions

let grammar_valid (grammar: regular_tree_grammar) =
    NonTerminalSet.mem grammar.start_non_terminal grammar.non_terminals &&
    productions_valid grammar.non_terminals grammar.ranked_alphabet grammar.productions

let non_term_label non_term =
    let NonTerminal(label) = non_term in label

let symbol_label symbol =
    let Symbol(label) = symbol in label

let print_non_terms set =
    let () = NonTerminalSet.iter (fun non_term -> Printf.printf "%s " (non_term_label non_term)) set in
    Printf.printf "\n"

let print_symbol s arity =
    Printf.printf "(%s, %i)\n" (symbol_label s) arity

let print_ranked_alpha alphabet =
    SymbolMap.iter print_symbol alphabet

let non_terms = NonTerminalSet.of_list [NonTerminal("t"); NonTerminal("v")]
let alpha = SymbolMap.of_seq (List.to_seq [(Symbol("if"), 3); (Symbol("true"), 0); (Symbol("false"), 0)])
let start = NonTerminal("t")

let productions = NonTerminalMap.of_seq (List.to_seq
    [
        (NonTerminal("t"), SententialTreeSet.of_list [
            Node(Symbol("true"), []);
            Node(Symbol("false"), []);
            Node(Symbol("if"), [Leaf(NonTerminal("t")); Leaf(NonTerminal("t")); Leaf(NonTerminal("t"))])
        ]);
        (NonTerminal("v"), SententialTreeSet.of_list [
            Node(Symbol("true"), []);
            Node(Symbol("false"), [])
        ]);
    ]
)

let invalid_test_tree: tree = Node(Symbol("if"), [
    Node(Symbol("true"), [
        Node(Symbol("false"), [])]);
    Node(Symbol("false"), []);
    Node(Symbol("true"), [])])
let valid_test_tree: tree = Node(Symbol("if"), [
    Node(Symbol("true"), []);
    Node(Symbol("false"), []);
    Node(Symbol("true"), [])])

let valid_sent_tree: sentential_tree = Node(Symbol("if"), [
    Leaf(NonTerminal("t"));
    Leaf(NonTerminal("t"));
    Leaf(NonTerminal("t"))])

let tree_grammar = {
    non_terminals = non_terms;
    ranked_alphabet = alpha;
    start_non_terminal = start;
    productions = productions;
}

let () = print_newline ()
let () = Printf.printf "Valid Tree: %b\n" (tree_valid alpha valid_test_tree)
let () = Printf.printf "Invalid Tree: %b\n" (tree_valid alpha invalid_test_tree)
let () = Printf.printf "Valid Sent Tree: %b\n" (sent_tree_valid non_terms alpha valid_sent_tree)
let () = Printf.printf "Productions: %b\n" (productions_valid non_terms alpha productions)
let () = Printf.printf "Grammar: %b\n" (grammar_valid tree_grammar)
let () = print_non_terms non_terms
let () = print_ranked_alpha alpha
