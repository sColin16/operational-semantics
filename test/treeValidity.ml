open TestHelpers
open Example.Boolean

let single_node_tree = parse_boolean_tree True

(* If has arity 3, not 0 *)
let single_node_tree_bad_arity : BooleanGrammar.tree = `Symbol (If, [])
let large_tree = parse_boolean_tree (If (If (True, False, True), False, True))

let large_tree_bad_arity : BooleanGrammar.tree =
  `Symbol
    ( If,
      [
        `Symbol (False, []);
        (* True has arity 0, not 1 *)
        `Symbol (True, [ `Symbol (True, []) ]);
        `Symbol (True, []);
      ] )

let non_term_sent_tree = parse_boolean_sent_tree T
let large_sent_tree = parse_boolean_sent_tree (If (False, V, If (True, T, V)))

let bad_arity_sent_tree : BooleanGrammar.sent_tree =
  `Symbol
    ( If,
      [
        `Symbol (False, []);
        (* If has arity 3, not 2 *)
        `Symbol (If, [ `Symbol (True, []); `NonTerminal T ]);
        `NonTerminal V;
      ] )

let assert_tree_in_alpha expected name tree =
  assert_bool name expected (BooleanGrammar.tree_in_alphabet tree)

let assert_sent_tree_in_alpha expected name sent_tree =
  assert_bool name expected (BooleanGrammar.sent_tree_in_alphabet sent_tree)

let () =
  let open Alcotest in
  run "Tree Validity"
    [
      ( "tree validity",
        [
          assert_tree_in_alpha true "single node tree" single_node_tree;
          assert_tree_in_alpha true "large tree" large_tree;
          assert_tree_in_alpha false "single node tree bad arity"
            single_node_tree_bad_arity;
          assert_tree_in_alpha false "large tree bad arity" large_tree_bad_arity;
        ] );
      ( "sentential tree validity",
        [
          assert_sent_tree_in_alpha true "non term sent tree" non_term_sent_tree;
          assert_sent_tree_in_alpha true "large sent tree" large_sent_tree;
          assert_sent_tree_in_alpha false "sent tree bad arity"
            bad_arity_sent_tree;
          assert_sent_tree_in_alpha true "single node sent tree"
            (single_node_tree :> BooleanGrammar.sent_tree);
          assert_sent_tree_in_alpha true "large tree as sent tree"
            (large_tree :> BooleanGrammar.sent_tree);
          assert_sent_tree_in_alpha false
            "single node tree bad arity as sent tree"
            (single_node_tree_bad_arity :> BooleanGrammar.sent_tree);
          assert_sent_tree_in_alpha false "large tree bad arity as sent tree"
            (large_tree_bad_arity :> BooleanGrammar.sent_tree);
        ] );
    ]
