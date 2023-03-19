type arithmetic_symbols = Zero | Succ | Plus
type arithmetic_non_terminals = T | V

let arithmetic_ranked_alphabet (symbol : arithmetic_symbols) =
  match symbol with Zero -> 0 | Succ -> 1 | Plus -> 2

module ArithmeticGrammar = RegularTreeGrammar.Make (struct
  type symbol = arithmetic_symbols
  type non_terminal = arithmetic_non_terminals

  let ranked_alphabet = arithmetic_ranked_alphabet
end)

type arithmetic_sent_tree =
  | Zero
  | Succ of arithmetic_sent_tree
  | Plus of arithmetic_sent_tree * arithmetic_sent_tree
  | T
  | V

type arithmetic_tree =
  | Zero
  | Succ of arithmetic_tree
  | Plus of arithmetic_tree * arithmetic_tree

let rec parse_arithmetic_sent_tree (tree : arithmetic_sent_tree) :
    ArithmeticGrammar.sent_tree =
  match tree with
  | Zero -> `Symbol (Zero, [])
  | Succ a -> `Symbol (Succ, List.map parse_arithmetic_sent_tree [ a ])
  | Plus (a, b) -> `Symbol (Plus, List.map parse_arithmetic_sent_tree [ a; b ])
  | T -> `NonTerminal T
  | V -> `NonTerminal V

let rec parse_arithmetic_tree (tree : arithmetic_tree) : ArithmeticGrammar.tree
    =
  match tree with
  | Zero -> `Symbol (Zero, [])
  | Succ a -> `Symbol (Succ, List.map parse_arithmetic_tree [ a ])
  | Plus (a, b) -> `Symbol (Plus, List.map parse_arithmetic_tree [ a; b ])

let parse_arithmetic_productions
    (productions : arithmetic_non_terminals -> arithmetic_sent_tree list) :
    arithmetic_non_terminals -> ArithmeticGrammar.sent_tree list =
 fun non_term -> List.map parse_arithmetic_sent_tree (productions non_term)

let primary_arithmetic_productions =
  parse_arithmetic_productions (fun non_term ->
      match non_term with
      | T -> [ Zero; Succ T; Plus (T, T) ]
      | V -> [ Zero; Succ V ])

let primary_arithmetic_grammar =
  ArithmeticGrammar.create_grammar T primary_arithmetic_productions
