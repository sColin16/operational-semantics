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

module BooleanGrammar = RegularTreeGrammar.TreeGrammar.Make (BooleanAlphabet)

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

type boolean_pattern_tree =
  | True
  | False
  | Not of boolean_pattern_tree
  | And of boolean_pattern_tree * boolean_pattern_tree
  | Or of boolean_pattern_tree * boolean_pattern_tree
  | If of boolean_pattern_tree * boolean_pattern_tree * boolean_pattern_tree
  | T of int
  | V of int
  | A of int
  | B of int

let rec parse_boolean_tree (tree : boolean_tree) : BooleanGrammar.Tree.t =
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

module BooleanSyntax = struct
  module TreeGrammar = BooleanGrammar

  let start_non_terminal : TreeGrammar.non_terminal = T
  let productions = primary_boolean_productions
end

module BooleanSemantics = RegularTreeGrammar.TreeSemantics.Make (BooleanSyntax)

let rec parse_boolean_pattern_tree (tree : boolean_pattern_tree) :
    BooleanSemantics.PatternTree.t =
  let open BooleanSemantics.PatternTree in
  match tree with
  | True -> symbol True []
  | False -> symbol False []
  | Not a -> symbol Not (List.map parse_boolean_pattern_tree [ a ])
  | And (a, b) -> symbol And (List.map parse_boolean_pattern_tree [ a; b ])
  | Or (a, b) -> symbol Or (List.map parse_boolean_pattern_tree [ a; b ])
  | If (a, b, c) -> symbol If (List.map parse_boolean_pattern_tree [ a; b; c ])
  | T a -> non_terminal T a
  | V a -> non_terminal V a
  | A a -> non_terminal A a
  | B a -> non_terminal B a

let parse_eval_relation
    (eval_relation : boolean_pattern_tree * boolean_pattern_tree) =
  let left, right = eval_relation in
  (parse_boolean_pattern_tree left, parse_boolean_pattern_tree right)

let parse_eval_rule eval_rule =
  let premises, conclusion = eval_rule in
  let parsed_premises = List.map parse_eval_relation premises in
  let parsed_conclusion = parse_eval_relation conclusion in
  (parsed_premises, parsed_conclusion)

let parse_eval_rules eval_rules = List.map parse_eval_rule eval_rules

let simple_boolean_eval_rules =
  parse_eval_rules
    [
      ([], (If (True, T 1, T 2), T 1));
      ([], (If (False, T 1, T 2), T 2));
      ([ (T 1, T 2) ], (If (T 1, T 3, T 4), If (T 2, T 3, T 4)));
    ]

let complete_boolean_eval_rules =
  parse_eval_rules
    [
      (* Axioms: the preference here is to evaluate on the LHS side first *)
      ([], (Not True, False));
      ([], (Not False, True));
      ([], (And (False, T 1), False));
      ([], (And (True, T 1), T 1));
      ([], (Or (False, T 1), T 1));
      ([], (Or (True, T 1), True));
      ([], (If (True, T 1, T 2), T 1));
      ([], (If (False, T 1, T 2), T 2));

      (* Inference rules: follow axioms and try to evaluate left argument *)
      ([ (T 1, T 2) ], (Not (T 1), Not (T 2)));
      ([ (T 1, T 2) ], (And (T 1, T 3), And (T 2, T 3)));
      ([ (T 1, T 2) ], (Or (T 1, T 3), And (T 2, T 3)));
      ([ (T 1, T 2) ], (If (T 1, T 3, T 4), If (T 2, T 3, T 4)));
    ]

let simple_boolean_semantics =
  BooleanSemantics.Semantics.of_rules simple_boolean_eval_rules

let complete_boolean_semantics =
  BooleanSemantics.Semantics.of_rules complete_boolean_eval_rules