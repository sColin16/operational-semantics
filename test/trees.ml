open Regular_tree_language
open Common

let small_valid_tree: tree = Node(Symbol("true"), [])
let small_bad_name_tree: tree = Node(Symbol("invalid_symbol"), [])
let small_bad_arity_tree: tree = Node(Symbol("if"), [Node(Symbol("true"), []); Node(Symbol("true"), [])])
let large_valid_tree: tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Node(Symbol("true"), []);
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("false"), []);
    Node(Symbol("true"), [])
  ]);
  Node(Symbol("true"), [])
])

let large_bad_name_tree: tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Node(Symbol("true"), []);
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("bad_name"), []);
    Node(Symbol("true"), [])
  ]);
  Node(Symbol("true"), [])
])

let large_bad_arity_tree: tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Node(Symbol("true"), []);
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("bad_name"), []);
    Node(Symbol("true"), [])
  ]);
  Node(Symbol("true"), [])
])

let valid_sent_tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Leaf(NonTerminal("a"));
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("false"), []);
    Node(Symbol("not"), [
      Leaf(NonTerminal("b"))
    ])
  ]);
  Leaf(NonTerminal("c"))
])

let bad_symbol_sent_tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Leaf(NonTerminal("a"));
    Node(Symbol("false"), [])]);
  Node(Symbol("bad_name"), [
    Node(Symbol("false"), []);
    Node(Symbol("not"), [
      Leaf(NonTerminal("b"))
    ])
  ]);
  Leaf(NonTerminal("c"))
])

let bad_non_terminal_tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Leaf(NonTerminal("a"));
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("false"), []);
    Node(Symbol("not"), [
      Leaf(NonTerminal("bad_non_term"))
    ])
  ]);
  Leaf(NonTerminal("c"))
])

let bad_arity_sent_tree = Node(Symbol("if"), [
  Node(Symbol("or"), [
    Leaf(NonTerminal("a"));
    Node(Symbol("false"), [])]);
  Node(Symbol("and"), [
    Node(Symbol("false"), []);
    Node(Symbol("not"), [
      Leaf(NonTerminal("c"));
      Leaf(NonTerminal("a"))
    ])
  ]);
  Leaf(NonTerminal("c"))
])

let test_small_valid () =
  Alcotest.(check bool) "valid single node tree" true (tree_valid alpha small_valid_tree)

let test_bad_name_invalid () =
  Alcotest.(check bool) "node with invalid symbol" false (tree_valid alpha small_bad_name_tree)

let test_bad_arity_invalid () =
  Alcotest.(check bool) "tree with wrong arity" false (tree_valid alpha small_bad_arity_tree)

let test_large_tree () =
  Alcotest.(check bool) "large valid tree" true (tree_valid alpha large_valid_tree)

let test_large_bad_name () =
  Alcotest.(check bool) "large bad symbol" false (tree_valid alpha large_bad_name_tree)

let test_large_bad_arity () =
  Alcotest.(check bool) "large bad arity" false (tree_valid alpha large_bad_arity_tree)

let test_valid_sent () =
  Alcotest.(check bool) "valid sent tree" true (sent_tree_valid non_terms alpha valid_sent_tree)

let test_bad_symbol_sent () =
  Alcotest.(check bool) "bad symbol sent tree" false (sent_tree_valid non_terms alpha bad_symbol_sent_tree)
let test_bad_non_term () =
  Alcotest.(check bool) "bad non terminal" false (sent_tree_valid non_terms alpha bad_non_terminal_tree)

let test_bad_arity () =
  Alcotest.(check bool) "bad arity sent tree" false (sent_tree_valid non_terms alpha bad_arity_sent_tree)

let () =
  let open Alcotest in
    run "Regular Trees" [
      "tree_valid", [
        test_case "simple valid tree" `Quick test_small_valid;
        test_case "simple bad name" `Quick test_bad_name_invalid;
        test_case "simple bad arity" `Quick test_bad_arity_invalid;
        test_case "large valid tree" `Quick test_large_tree;
        test_case "large bad name" `Quick test_large_bad_name;
        test_case "large bad arity" `Quick test_large_bad_arity;
      ];
      "sent_tree_valid", [
        test_case "valid tree" `Quick test_valid_sent;
        test_case "bad symbol name" `Quick test_bad_symbol_sent;
        test_case "bad non terminal name" `Quick test_bad_non_term;
        test_case "bad arity" `Quick test_bad_arity
      ];
    ]