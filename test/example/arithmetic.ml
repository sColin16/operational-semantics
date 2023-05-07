module ArithmeticAlphabet = struct
  module RankedAlphabet = struct
    type symbol = Zero | Succ | Plus

    let arity symbol = match symbol with Zero -> 0 | Succ -> 1 | Plus -> 2
  end

  type non_terminal = T | V
end

module ArithmeticGrammar = RegularTreeGrammar.TreeGrammar.Make (ArithmeticAlphabet)

type arithmetic_tree =
  | Zero
  | Succ of arithmetic_tree
  | Plus of arithmetic_tree * arithmetic_tree

type arithmetic_sent_tree =
  | Zero
  | Succ of arithmetic_sent_tree
  | Plus of arithmetic_sent_tree * arithmetic_sent_tree
  | T
  | V

let rec parse_arithmetic_tree (tree : arithmetic_tree) :
    ArithmeticGrammar.Tree.t =
  let open ArithmeticGrammar.Tree in
  match tree with
  | Zero -> node Zero []
  | Succ a -> node Succ (List.map parse_arithmetic_tree [ a ])
  | Plus (a, b) -> node Plus (List.map parse_arithmetic_tree [ a; b ])

let rec parse_arithmetic_sent_tree (tree : arithmetic_sent_tree) :
    ArithmeticGrammar.SententialTree.t =
  let open ArithmeticGrammar.SententialTree in
  match tree with
  | Zero -> symbol Zero []
  | Succ a -> symbol Succ (List.map parse_arithmetic_sent_tree [ a ])
  | Plus (a, b) -> symbol Plus (List.map parse_arithmetic_sent_tree [ a; b ])
  | T -> non_terminal T
  | V -> non_terminal V

let parse_arithmetic_productions
    (productions : ArithmeticAlphabet.non_terminal -> arithmetic_sent_tree list)
    :
    ArithmeticAlphabet.non_terminal ->
    ArithmeticGrammar.SententialTree.t list =
 fun non_term -> List.map parse_arithmetic_sent_tree (productions non_term)

let primary_arithmetic_productions =
  parse_arithmetic_productions (fun non_term ->
      match non_term with
      | T -> [ Zero; Succ T; Plus (T, T) ]
      | V -> [ Zero; Succ V ])

let primary_arithmetic_grammar =
  ArithmeticGrammar.Grammar.of_productions T primary_arithmetic_productions
