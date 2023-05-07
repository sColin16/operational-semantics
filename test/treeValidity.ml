open TestHelpers
open Example.Boolean
open BooleanGrammar

let single_node_tree () = ignore (parse_boolean_tree True)
let single_node_sent_tree () = ignore (parse_boolean_sent_tree True)

let non_term_sent_tree () = ignore (parse_boolean_sent_tree T)

(* If has arity 3, not 0 *)
let single_node_tree_bad_arity () = ignore (Tree.node If [])
let single_node_sent_tree_bad_arity () = ignore (SententialTree.symbol If [])

let assert_tree_valid expected name tree_func =
  if expected then assert_no_raises name tree_func
  else
    assert_raises name
      (Invalid_argument
         "Wrong number of children provided for symbol")
      tree_func

let assert_sent_tree_valid expected name tree_func =
  if expected then assert_no_raises name tree_func
  else
    assert_raises name
      (Invalid_argument
         "Wrong number of children provided for symbol")
      tree_func

let () =
  let open Alcotest in
  run "Tree Validity"
    [
      ( "tree validity",
        [
          assert_tree_valid true "single node tree" single_node_tree;
          assert_tree_valid false "single node tree bad arity"
            single_node_tree_bad_arity;
        ] );
      ( "sentential tree validity",
        [
          assert_sent_tree_valid true "single node sent tree"
            single_node_sent_tree;
          assert_sent_tree_valid true "non term sent tree" non_term_sent_tree;
          assert_sent_tree_valid false
            "single node tree bad arity as sent tree"
            single_node_sent_tree_bad_arity;
        ] );
    ]
