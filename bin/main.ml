open Regular_tree_language

let non_terms = NonTerminalSet.of_list [NonTerminal("t"); NonTerminal("v")]
let alpha = SymbolMap.of_seq (List.to_seq [(Symbol("if"), 3); (Symbol("true"), 0); (Symbol("false"), 0)])
let start = NonTerminal("t")

let productions = NonTerminalMap.of_seq (List.to_seq
    [
        (NonTerminal("t"), SententialTreeSet.of_list [
            Node(Symbol("true"), []);
            Node(Symbol("false"), []);
            Node(Symbol("if"), [Leaf(NonTerminal("t")); Leaf(NonTerminal("t")); Leaf(NonTerminal("t"))])
        ]);
        (NonTerminal("v"), SententialTreeSet.of_list [
            Node(Symbol("true"), []);
            Node(Symbol("false"), [])
        ]);
    ]
)

let invalid_test_tree: tree = Node(Symbol("if"), [
    Node(Symbol("true"), [
        Node(Symbol("false"), [])]);
    Node(Symbol("false"), []);
    Node(Symbol("true"), [])])
let valid_test_tree: tree = Node(Symbol("if"), [
    Node(Symbol("true"), []);
    Node(Symbol("false"), []);
    Node(Symbol("true"), [])])

let valid_sent_tree: sent_tree = Node(Symbol("if"), [
    Leaf(NonTerminal("t"));
    Leaf(NonTerminal("t"));
    Leaf(NonTerminal("t"))])

let tree_grammar = {
    non_terminals = non_terms;
    ranked_alphabet = alpha;
    start_non_terminal = start;
    productions = productions;
}

let () = print_newline ()
let () = Printf.printf "Valid Tree: %b\n" (tree_valid alpha valid_test_tree)
let () = Printf.printf "Invalid Tree: %b\n" (tree_valid alpha invalid_test_tree)
let () = Printf.printf "Valid Sent Tree: %b\n" (sent_tree_valid non_terms alpha valid_sent_tree)
let () = Printf.printf "Productions: %b\n" (productions_valid non_terms alpha productions)
let () = Printf.printf "Grammar: %b\n" (grammar_valid tree_grammar)
let () = print_non_terms non_terms
let () = print_ranked_alpha alpha
