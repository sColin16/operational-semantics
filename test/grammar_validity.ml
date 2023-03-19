open Common

let invalid_productions (non_term : test_non_terminals) :
    TestGrammar.sent_tree list =
  match non_term with
  | T -> [ `Symbol (If, [ `NonTerminal T; `NonTerminal T; `NonTerminal T ]) ]
  | V -> [ `Symbol (True, [ `NonTerminal V ]) ]
  | _ -> []

let deep_valid_productions (non_term : test_non_terminals) :
    TestGrammar.sent_tree list =
  match non_term with
  | T ->
      [
        `NonTerminal T;
        `Symbol
          ( If,
            [
              `Symbol
                (If, [ `NonTerminal V; `Symbol (True, []); `Symbol (False, []) ]);
              `Symbol (True, []);
              `NonTerminal T;
            ] );
      ]
  | V -> [ `Symbol (If, [ `NonTerminal T; `NonTerminal V; `NonTerminal A ]) ]
  | A -> [ `Symbol (If, [ `NonTerminal V; `NonTerminal A; `NonTerminal B ]) ]
  | B -> [ `Symbol (True, []) ]

let deep_invalid_productions (non_term : test_non_terminals) :
    TestGrammar.sent_tree list =
  match non_term with
  | T ->
      [
        `Symbol
          ( If,
            [
              `Symbol
                (If, [ `NonTerminal V; `Symbol (True, []); `Symbol (False, []) ]);
              `Symbol (True, []);
              `NonTerminal T;
            ] );
      ]
  | V -> [ `Symbol (If, [ `NonTerminal T; `NonTerminal V; `NonTerminal A ]) ]
  | A -> [ `Symbol (If, [ `NonTerminal V; `NonTerminal A; `NonTerminal B ]) ]
  (* True has arity 0, not 1 *)
  | B -> [ `Symbol (True, [ `NonTerminal B ]) ]

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
          assert_grammar_valid true "test grammar is valid for T"
            (module TestGrammar)
            T test_productions;
          assert_grammar_valid true "test grammar is valid for V"
            (module TestGrammar)
            V test_productions;
          assert_grammar_valid true "boolean grammar is valid for T"
            (module BooleanGrammar)
            T boolean_productions;
          assert_grammar_valid true "boolean grammar is valid for V"
            (module BooleanGrammar)
            V boolean_productions;
          assert_grammar_valid true "invalid production must be reachable"
            (module TestGrammar)
            T invalid_productions;
          assert_grammar_valid false
            "immediate reachable invalid productions fail"
            (module TestGrammar)
            V invalid_productions;
          assert_grammar_valid true "deep grammar is valid"
            (module TestGrammar)
            T deep_valid_productions;
          assert_grammar_valid false "deep invalid grammar"
            (module TestGrammar)
            T deep_invalid_productions;
        ] );
    ]
