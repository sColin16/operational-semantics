type boolean_symbols = True | False | Not | And | Or | If
type boolean_non_terminals = T | V | A | B

let boolean_ranked_alphabet (symbol : boolean_symbols) =
  match symbol with
  | True -> 0
  | False -> 0
  | Not -> 1
  | And -> 2
  | Or -> 2
  | If -> 3

module BooleanGrammar = RegularTreeGrammar.Make (struct
  type symbol = boolean_symbols
  type non_terminal = boolean_non_terminals

  let ranked_alphabet = boolean_ranked_alphabet
end)

type boolean_tree =
  | True
  | False
  | Not of boolean_tree
  | And of boolean_tree * boolean_tree
  | Or of boolean_tree * boolean_tree
  | If of boolean_tree * boolean_tree * boolean_tree

type boolean_sent_tree =
  | True
  | False
  | Not of boolean_sent_tree
  | And of boolean_sent_tree * boolean_sent_tree
  | Or of boolean_sent_tree * boolean_sent_tree
  | If of boolean_sent_tree * boolean_sent_tree * boolean_sent_tree
  | T
  | V
  | A
  | B

let rec parse_boolean_sent_tree (tree : boolean_sent_tree) :
    BooleanGrammar.sent_tree =
  match tree with
  | True -> `Symbol (True, [])
  | False -> `Symbol (False, [])
  | Not a -> `Symbol (Not, List.map parse_boolean_sent_tree [ a ])
  | And (a, b) -> `Symbol (And, List.map parse_boolean_sent_tree [ a; b ])
  | Or (a, b) -> `Symbol (Or, List.map parse_boolean_sent_tree [ a; b ])
  | If (a, b, c) -> `Symbol (If, List.map parse_boolean_sent_tree [ a; b; c ])
  | T -> `NonTerminal T
  | V -> `NonTerminal V
  | A -> `NonTerminal A
  | B -> `NonTerminal B

let rec parse_boolean_tree (tree : boolean_tree) : BooleanGrammar.tree =
  match tree with
  | True -> `Symbol (True, [])
  | False -> `Symbol (False, [])
  | Not a -> `Symbol (Not, List.map parse_boolean_tree [ a ])
  | And (a, b) -> `Symbol (And, List.map parse_boolean_tree [ a; b ])
  | Or (a, b) -> `Symbol (Or, List.map parse_boolean_tree [ a; b ])
  | If (a, b, c) -> `Symbol (If, List.map parse_boolean_tree [ a; b; c ])

let parse_boolean_productions
    (productions : boolean_non_terminals -> boolean_sent_tree list) :
    boolean_non_terminals -> BooleanGrammar.sent_tree list =
 fun non_term -> List.map parse_boolean_sent_tree (productions non_term)

let primary_boolean_productions =
  parse_boolean_productions (fun non_term ->
      match non_term with
      | T -> [ True; False; Not T; And (T, T); Or (T, T); If (T, T, T) ]
      | V -> [ True; False ]
      | _ -> [])

let primary_boolean_grammar =
  BooleanGrammar.create_grammar T primary_boolean_productions
