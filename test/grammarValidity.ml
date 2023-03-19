open TestHelpers
open Example.Boolean
open Example.Arithmetic

let invalid_productions (non_term : boolean_non_terminals) =
  match non_term with
  | T -> List.map parse_boolean_sent_tree [ If (T, T, T) ]
  | V -> [ `Symbol (True, [ `NonTerminal V ]) ]
  | _ -> []

let deep_valid_productions =
  parse_boolean_productions (fun non_term ->
      match non_term with
      | T -> [ T; If (If (V, True, False), True, T) ]
      | V -> [ If (T, V, A) ]
      | A -> [ If (V, A, B) ]
      | B -> [ True ])

let deep_invalid_productions (non_term : boolean_non_terminals) :
    BooleanGrammar.sent_tree list =
  match non_term with
  | T -> List.map parse_boolean_sent_tree [ If (If (V, True, False), True, T) ]
  | V -> List.map parse_boolean_sent_tree [ If (T, V, A) ]
  | A -> List.map parse_boolean_sent_tree [ If (V, A, B) ]
  (* True has arity 0, not 1 *)
  | B -> [ `Symbol (True, [ `NonTerminal B ]) ]

let null_productions non_term = match non_term with _ -> []

let assert_grammar_valid (type non_terminal symbol) expected_valid name
    (grammar_module :
      (module RegularTreeGrammar.REG_TREE_GRAMMAR
         with type non_terminal = non_terminal
          and type symbol = symbol)) start_symbol productions =
  let module GrammarModule = (val grammar_module) in
  let test_func () =
    ignore (GrammarModule.create_grammar start_symbol productions)
  in
  let expected_exn =
    GrammarModule.Invalid_production
      "One of more of the provided productions contained invalid sentential \
       trees"
  in
  if expected_valid then assert_no_raises name test_func
  else assert_raises name expected_exn test_func

let () =
  let open Alcotest in
  run "Regular tree grammars"
    [
      ( "grammar validity",
        [
          assert_grammar_valid true "boolean grammar is valid for T"
            (module BooleanGrammar)
            T primary_boolean_productions;
          assert_grammar_valid true "boolean grammar is valid for V"
            (module BooleanGrammar)
            V primary_boolean_productions;
          assert_grammar_valid true "arithmetic grammar is valid for T"
            (module ArithmeticGrammar)
            T primary_arithmetic_productions;
          assert_grammar_valid true "arithmetic grammar is valid for V"
            (module ArithmeticGrammar)
            V primary_arithmetic_productions;
          assert_grammar_valid true "invalid production must be reachable"
            (module BooleanGrammar)
            T invalid_productions;
          assert_grammar_valid false
            "immediate reachable invalid productions fail"
            (module BooleanGrammar)
            V invalid_productions;
          assert_grammar_valid true "deep grammar is valid"
            (module BooleanGrammar)
            T deep_valid_productions;
          assert_grammar_valid false "deep invalid grammar"
            (module BooleanGrammar)
            T deep_invalid_productions;
          assert_grammar_valid true "null grammar valid"
            (module BooleanGrammar)
            T null_productions;
        ] );
    ]
