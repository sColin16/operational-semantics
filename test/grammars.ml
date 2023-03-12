open Common

let test_invalid_productions (non_term : test_non_terminals) :
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

let test_test_grammar_t () =
  Alcotest.(check bool)
    "Test grammar should not throw an exception on creation" true
    (try
       let _ = TestGrammar.create_grammar T test_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

let test_test_grammar_v () =
  Alcotest.(check bool)
    "Test grammar should not throw an exception on creation" true
    (try
       let _ = TestGrammar.create_grammar V test_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

let test_boolean_grammar_t () =
  Alcotest.(check bool)
    "Boolean grammar should not throw an exception on creation" true
    (try
       let _ = BooleanGrammar.create_grammar T boolean_productions in
       true
     with BooleanGrammar.Invalid_production _ -> false)

let test_boolean_grammar_v () =
  Alcotest.(check bool)
    "Boolean grammar should not throw an exception on creation" true
    (try
       let _ = BooleanGrammar.create_grammar V boolean_productions in
       true
     with BooleanGrammar.Invalid_production _ -> false)

let test_invalid_but_unreachable () =
  Alcotest.(check bool)
    "grammar with invalid productions that are unreachable is valid" true
    (try
       let _ = TestGrammar.create_grammar T test_invalid_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

let test_immediate_invalid () =
  Alcotest.(check bool)
    "grammar with immediately invalid productions" false
    (try
       let _ = TestGrammar.create_grammar V test_invalid_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

let test_deep_valid_productions () =
  Alcotest.(check bool)
    "large grammar is considered valid" true
    (try
       let _ = TestGrammar.create_grammar T deep_valid_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

let test_deep_invalid_productions () =
  Alcotest.(check bool)
    "non-immediate invalid production is detected" false
    (try
       let _ = TestGrammar.create_grammar T deep_invalid_productions in
       true
     with TestGrammar.Invalid_production _ -> false)

(**
Is element functions:
  We need to test grammars with loops: both trivial loops, and larger non-trivial loops
  I need to come up with interesting tree grammars and just test a bunch of stuff
  Some of the grammars should be flat, as I think most of the ones I work with will be
  Some of the grammar should not be flat since I have the code to handle that as well
  I should see if I can trigger all of the different match cases here, in some way
  Can I get some randomly generated grammars? Or randomly generate trees that are or are not in the language
  I can have single-production grammars to really test out the single sent tree operations
  For example, one test that passes to check if a sent tree is derivable to a tree, and another that goes the other way and should fail
*)

let () =
  let open Alcotest in
  run "Regular tree grammars"
    [
      ( "grammar validity",
        [
          test_case "test grammar is valid for T" `Quick test_test_grammar_t;
          test_case "test grammar is valid for V" `Quick test_test_grammar_v;
          test_case "boolean grammar is valid for T" `Quick
            test_boolean_grammar_t;
          test_case "boolean grammar is valid for V" `Quick
            test_boolean_grammar_v;
          test_case "invalid productions must be reachable" `Quick
            test_invalid_but_unreachable;
          test_case "immediate reachable invalid productions fail" `Quick
            test_immediate_invalid;
          test_case "deep grammar is valid" `Quick test_deep_valid_productions;
          test_case "deep invalid grammar" `Quick test_deep_invalid_productions;
        ] );
    ]
