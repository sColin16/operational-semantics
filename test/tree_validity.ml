open Common

let single_node_tree : TestGrammar.tree = `Symbol (True, [])

(* If has arity 3, not 0 *)
let single_node_tree_bad_arity : TestGrammar.tree = `Symbol (If, [])

let large_tree : TestGrammar.tree =
  `Symbol
    ( If,
      [
        `Symbol
          (If, [ `Symbol (True, []); `Symbol (False, []); `Symbol (True, []) ]);
        `Symbol (False, []);
        `Symbol (True, []);
      ] )

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

let assert_tree_in_alpha expected name tree =
  assert_bool name expected (TestGrammar.tree_in_alphabet tree)

let assert_sent_tree_in_alpha expected name sent_tree =
  assert_bool name expected (TestGrammar.sent_tree_in_alphabet sent_tree)

let () =
  let open Alcotest in
  run "Regular Trees"
    [
      ( "tree_in_alphabet",
        [
          assert_tree_in_alpha true "single node tree" single_node_tree;
          assert_tree_in_alpha true "large tree" large_tree;
          assert_tree_in_alpha false "single node tree bad arity"
            single_node_tree_bad_arity;
          assert_tree_in_alpha false "large tree bad arity" large_tree_bad_arity;
        ] );
      ( "sent_tree_in_alphabet",
        [
          assert_sent_tree_in_alpha true "non term sent tree" non_term_sent_tree;
          assert_sent_tree_in_alpha true "large sent tree" large_sent_tree;
          assert_sent_tree_in_alpha false "sent tree bad arity"
            bad_arity_sent_tree;
          assert_sent_tree_in_alpha true "single node sent tree"
            (single_node_tree :> TestGrammar.sent_tree);
          assert_sent_tree_in_alpha true "large tree as sent tree"
            (large_tree :> TestGrammar.sent_tree);
          assert_sent_tree_in_alpha false
            "single node tree bad arity as sent tree"
            (single_node_tree_bad_arity :> TestGrammar.sent_tree);
          assert_sent_tree_in_alpha false "large tree bad arity as sent tree"
            (large_tree_bad_arity :> TestGrammar.sent_tree);
        ] );
    ]
