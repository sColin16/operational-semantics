module AbstractAlphabet = struct
  module RankedAlphabet = struct
    type symbol = A | B | C | D

    let arity symbol = match symbol with A -> 0 | B -> 1 | C -> 2 | D -> 3
  end

  type non_terminal = F | G | H | I
end

module AbstractGrammar = RegularTreeGrammar.TreeGrammar.Make (AbstractAlphabet)

type abstract_tree =
  | A
  | B of abstract_tree
  | C of abstract_tree * abstract_tree
  | D of abstract_tree * abstract_tree * abstract_tree

type abstract_sent_tree =
  | A
  | B of abstract_sent_tree
  | C of abstract_sent_tree * abstract_sent_tree
  | D of abstract_sent_tree * abstract_sent_tree * abstract_sent_tree
  | F
  | G
  | H
  | I

let rec parse_abstract_tree (tree : abstract_tree) :
    AbstractGrammar.Tree.t =
  let open AbstractGrammar.Tree in
  match tree with
  | A -> node A []
  | B a -> node B (List.map parse_abstract_tree [ a ])
  | C (a, b) -> node C (List.map parse_abstract_tree [ a; b ])
  | D (a, b, c) -> node D (List.map parse_abstract_tree [ a; b; c ])

let rec parse_abstract_sent_tree (tree : abstract_sent_tree) :
    AbstractGrammar.SententialTree.t =
  let open AbstractGrammar.SententialTree in
  match tree with
  | A -> symbol A []
  | B a -> symbol B (List.map parse_abstract_sent_tree [ a ])
  | C (a, b) -> symbol C (List.map parse_abstract_sent_tree [ a; b ])
  | D (a, b, c) -> symbol D (List.map parse_abstract_sent_tree [ a; b; c ])
  | F -> non_terminal F
  | G -> non_terminal G
  | H -> non_terminal H
  | I -> non_terminal I

let parse_abstract_productions
    (productions : AbstractAlphabet.non_terminal -> abstract_sent_tree list) :
    AbstractAlphabet.non_terminal ->
    AbstractGrammar.SententialTree.t list =
 fun non_term -> List.map parse_abstract_sent_tree (productions non_term)
