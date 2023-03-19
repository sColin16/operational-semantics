type abstract_symbols = A | B | C | D
type abstract_non_terminals = F | G | H | I

let abstract_ranked_alphabet (symbol : abstract_symbols) =
  match symbol with A -> 0 | B -> 1 | C -> 2 | D -> 3

module AbstractGrammar = RegularTreeGrammar.Make (struct
  type symbol = abstract_symbols
  type non_terminal = abstract_non_terminals

  let ranked_alphabet = abstract_ranked_alphabet
end)

type abstract_sent_tree =
  | A
  | B of abstract_sent_tree
  | C of abstract_sent_tree * abstract_sent_tree
  | D of abstract_sent_tree * abstract_sent_tree * abstract_sent_tree
  | F
  | G
  | H
  | I

type abstract_tree =
  | A
  | B of abstract_tree
  | C of abstract_tree * abstract_tree
  | D of abstract_tree * abstract_tree * abstract_tree

let rec parse_abstract_sent_tree (tree : abstract_sent_tree) :
    AbstractGrammar.sent_tree =
  match tree with
  | A -> `Symbol (A, [])
  | B a -> `Symbol (B, List.map parse_abstract_sent_tree [ a ])
  | C (a, b) -> `Symbol (C, List.map parse_abstract_sent_tree [ a; b ])
  | D (a, b, c) -> `Symbol (D, List.map parse_abstract_sent_tree [ a; b; c ])
  | F -> `NonTerminal F
  | G -> `NonTerminal G
  | H -> `NonTerminal H
  | I -> `NonTerminal I

let rec parse_abstract_tree (tree : abstract_tree) : AbstractGrammar.tree =
  match tree with
  | A -> `Symbol (A, [])
  | B a -> `Symbol (B, List.map parse_abstract_tree [ a ])
  | C (a, b) -> `Symbol (C, List.map parse_abstract_tree [ a; b ])
  | D (a, b, c) -> `Symbol (D, List.map parse_abstract_tree [ a; b; c ])

let parse_abstract_productions
    (productions : abstract_non_terminals -> abstract_sent_tree list) :
    abstract_non_terminals -> AbstractGrammar.sent_tree list =
 fun non_term -> List.map parse_abstract_sent_tree (productions non_term)
