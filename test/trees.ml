open Common

let single_node_tree : TestGrammar.tree = `Symbol (True, [])

let large_tree : TestGrammar.tree =
  `Symbol
    ( If,
      [
        `Symbol
          (If, [ `Symbol (True, []); `Symbol (False, []); `Symbol (True, []) ]);
        `Symbol (False, []);
        `Symbol (True, []);
      ] )

(* If has arity 3, not 0 *)
let single_node_tree_bad_arity : TestGrammar.tree = `Symbol (If, [])

let large_tree_bad_arity : TestGrammar.tree =
  `Symbol
    ( If,
      [
        `Symbol (False, []);
        (* True has arity 0, not 1 *)
        `Symbol (True, [ `Symbol (True, []) ]);
        `Symbol (True, []);
      ] )

let non_term_sent_tree : TestGrammar.sent_tree = `NonTerminal T

let large_sent_tree : TestGrammar.sent_tree =
  `Symbol
    ( If,
      [
        `Symbol (False, []);
        `NonTerminal V;
        `Symbol (If, [ `Symbol (True, []); `NonTerminal T; `NonTerminal V ]);
      ] )

let bad_arity_sent_tree : TestGrammar.sent_tree =
  `Symbol
    ( If,
      [
        `Symbol (False, []);
        (* If has arity 3, not 2 *)
        `Symbol (If, [ `Symbol (True, []); `NonTerminal T ]);
        `NonTerminal V;
      ] )

let test_single_node_tree () =
  Alcotest.(check bool)
    "valid single node tree is in alphabet" true
    (TestGrammar.tree_in_alphabet single_node_tree)

let test_large_tree () =
  Alcotest.(check bool)
    "valid large tree is in alphabet" true
    (TestGrammar.tree_in_alphabet large_tree)

let test_single_node_bad_arity_tree () =
  Alcotest.(check bool)
    "single node with bad arity not in alphabet" false
    (TestGrammar.tree_in_alphabet single_node_tree_bad_arity)

let test_large_tree_bad_arity () =
  Alcotest.(check bool)
    "tree with bad arity not in alphabet" false
    (TestGrammar.tree_in_alphabet large_tree_bad_arity)

let test_non_term_sent_tree () =
  Alcotest.(check bool)
    "sent tree of non terminal in alphabet" true
    (TestGrammar.sent_tree_in_alphabet non_term_sent_tree)

let test_large_sent_tree () =
  Alcotest.(check bool)
    "large valid sent tree in alphabet" true
    (TestGrammar.sent_tree_in_alphabet large_sent_tree)

let test_sent_tree_bad_arity () =
  Alcotest.(check bool)
    "sent tree with invalid arity not in alphabet" false
    (TestGrammar.sent_tree_in_alphabet bad_arity_sent_tree)

let test_single_node_tree_sent () =
  Alcotest.(check bool)
    "valid single node tree is in alphabet as sent tree" true
    (TestGrammar.sent_tree_in_alphabet
       (single_node_tree :> TestGrammar.sent_tree))

let test_large_tree_sent () =
  Alcotest.(check bool)
    "valid large tree is in alphabet as sent tree" true
    (TestGrammar.sent_tree_in_alphabet (large_tree :> TestGrammar.sent_tree))

let test_single_node_bad_arity_tree_sent () =
  Alcotest.(check bool)
    "single node as sent tree with bad arity not in alphabet" false
    (TestGrammar.sent_tree_in_alphabet
       (single_node_tree_bad_arity :> TestGrammar.sent_tree))

let test_large_tree_bad_arity_sent () =
  Alcotest.(check bool)
    "tree as sent tree with bad arity not in alphabet" false
    (TestGrammar.sent_tree_in_alphabet
       (large_tree_bad_arity :> TestGrammar.sent_tree))

let () =
  let open Alcotest in
  run "Regular Trees"
    [
      ( "tree_in_alphabet",
        [
          test_case "single node tree" `Quick test_single_node_tree;
          test_case "large tree" `Quick test_large_tree;
          test_case "single node tree bad arity" `Quick
            test_single_node_bad_arity_tree;
          test_case "large tree bad arity" `Quick test_large_tree_bad_arity;
        ] );
      ( "sent_tree_in_alphabet",
        [
          test_case "non term sent tree" `Quick test_non_term_sent_tree;
          test_case "large sent tree" `Quick test_large_sent_tree;
          test_case "sent tree bad arity" `Quick test_sent_tree_bad_arity;
          test_case "single node sent tree" `Quick test_single_node_tree_sent;
          test_case "large tree as sent tree" `Quick test_large_tree_sent;
          test_case "single node tree bad arity as sent tree" `Quick
            test_single_node_bad_arity_tree_sent;
          test_case "large tree bad arity as sent tree" `Quick
            test_large_tree_bad_arity_sent;
        ] );
    ]
