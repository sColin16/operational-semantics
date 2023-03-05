(**
 * TODO: Make these types and a regular grammar be a module like Set that can accept different types
 * This does add a fair amount of complexity with a ton of benefit, so I will probably hold off
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

type sent_tree =
    | Node of symbol * sent_tree list
    | Leaf of non_terminal

module SententialTreeSet = Set.Make(
    struct
        type t = sent_tree
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