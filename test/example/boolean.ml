module BooleanAlphabet = struct
  module RankedAlphabet = struct
    type symbol = True | False | Not | And | Or | If

    let arity symbol =
      match symbol with
      | True -> 0
      | False -> 0
      | Not -> 1
      | And -> 2
      | Or -> 2
      | If -> 3
  end

  type non_terminal = T | V | A | B
end

module BooleanGrammar = RegularTreeGrammar.TreeGrammar.Make(BooleanAlphabet)

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

let rec parse_boolean_tree (tree : boolean_tree) :
    BooleanGrammar.Tree.t =
  let open BooleanGrammar.Tree in
  match tree with
  | True -> node True []
  | False -> node False []
  | Not a -> node Not (List.map parse_boolean_tree [ a ])
  | And (a, b) -> node And (List.map parse_boolean_tree [ a; b ])
  | Or (a, b) -> node Or (List.map parse_boolean_tree [ a; b ])
  | If (a, b, c) -> node If (List.map parse_boolean_tree [ a; b; c ])

let rec parse_boolean_sent_tree (tree : boolean_sent_tree) :
    BooleanGrammar.SententialTree.t =
  let open BooleanGrammar.SententialTree in
  match tree with
  | True -> symbol True []
  | False -> symbol False []
  | Not a -> symbol Not (List.map parse_boolean_sent_tree [ a ])
  | And (a, b) -> symbol And (List.map parse_boolean_sent_tree [ a; b ])
  | Or (a, b) -> symbol Or (List.map parse_boolean_sent_tree [ a; b ])
  | If (a, b, c) -> symbol If (List.map parse_boolean_sent_tree [ a; b; c ])
  | T -> non_terminal T
  | V -> non_terminal V
  | A -> non_terminal A
  | B -> non_terminal B

let parse_boolean_productions
    (productions : BooleanAlphabet.non_terminal -> boolean_sent_tree list) :
    BooleanAlphabet.non_terminal -> BooleanGrammar.SententialTree.t list =
 fun non_term -> List.map parse_boolean_sent_tree (productions non_term)

let primary_boolean_productions =
  parse_boolean_productions (fun non_term ->
      match non_term with
      | T -> [ True; False; Not T; And (T, T); Or (T, T); If (T, T, T) ]
      | V -> [ True; False ]
      | _ -> [])

let primary_boolean_grammar =
  BooleanGrammar.Grammar.of_productions T primary_boolean_productions
