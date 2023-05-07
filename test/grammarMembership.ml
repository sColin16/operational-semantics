open TestHelpers
open Example.Abstract

let assert_is_element name tree grammar =
  assert_true name (AbstractGrammar.Grammar.is_element tree grammar)

let assert_not_element name tree grammar =
  assert_false name (AbstractGrammar.Grammar.is_element tree grammar)

let null_productions non_term = match non_term with _ -> []
let null_grammar = AbstractGrammar.Grammar.of_productions F null_productions
let single_node_tree = parse_abstract_tree A
let simple_tree = parse_abstract_tree (D (C (B A, A), B A, A))

let empty_no_loops_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ G; C (G, H) ]
      | G -> [ D (B A, H, H); I ]
      | H -> []
      | I -> [])

let empty_no_loop_grammar =
  AbstractGrammar.Grammar.of_productions F empty_no_loops_productions

let empty_no_loop_tree1 = parse_abstract_tree (C (D (B A, A, B A), A))
let empty_no_loop_tree2 = parse_abstract_tree (D (B A, C (B A, A), A))

let empty_symbol_loops_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ G; H; B (C (A, F)); B (D (G, B A, I)) ]
      | G -> [ H; I; C (B F, B G) ]
      | H -> [ I; D (F, C (G, A), I) ]
      | I -> [])

let empty_symbol_loop_grammar =
  AbstractGrammar.Grammar.of_productions F empty_symbol_loops_productions

let empty_symbol_loop_tree1 =
  parse_abstract_tree (B (C (A, B (D (C (B (B A), B (B A)), B A, A)))))

let empty_symbol_loop_tree2 =
  parse_abstract_tree (B (D (D (B A, C (C (B A, B A), A), A), B A, C (A, A))))

let empty_symbol_loop_tree3 =
  parse_abstract_tree (C (B (D (A, C (D (A, A, A), A), A)), B A))

let empty_symbol_loop_tree4 =
  parse_abstract_tree (D (B (C (A, A)), C (C (B A, B A), A), A))

let empty_loop_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ F; I; H; D (H, A, H) ]
      | G -> [ G; F; I; C (G, C (H, C (F, A))) ]
      | H -> [ H; G; F; B (D (I, B F, A)) ]
      | I -> [ I; H; G; D (H, B I, A) ])

let empty_loop_grammar = AbstractGrammar.Grammar.of_productions F empty_loop_productions

let empty_loop_tree1 =
  parse_abstract_tree
    (D (B (D (D (A, B A, A), B A, A)), A, C (A, C (A, C (C (A, A), A)))))

let empty_loop_tree2 =
  parse_abstract_tree (C (D (A, A, B A), C (B (D (A, B A, A)), C (A, A))))

let empty_loop_tree3 =
  parse_abstract_tree
    (B (D (D (C (A, A), B (B (C (A, B A))), A), B (D (A, B A, A)), A)))

let empty_loop_tree4 =
  parse_abstract_tree
    (D (C (A, C (D (B A, B A, B A), C (C (A, C (A, A)), A))), B A, A))

let single_direct_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ D (A, D (C (B A, A), C (A, A), A), C (A, A)) ]
      | _ -> [])

let single_derivable_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ D (A, G, H) ]
      | G -> [ D (C (I, A), H, A) ]
      | H -> [ C (A, A) ]
      | I -> [ B A ])

let single_direct_grammar =
  AbstractGrammar.Grammar.of_productions F single_direct_productions

let single_derviable_grammar =
  AbstractGrammar.Grammar.of_productions F single_derivable_productions

let single_member_tree =
  parse_abstract_tree (D (A, D (C (B A, A), C (A, A), A), C (A, A)))

let single_nonmember_tree =
  parse_abstract_tree (D (A, D (C (A, B A), B A, C (A, A)), A))

let infinite_simple_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ A; G; C (A, I) ]
      | G -> [ B F; H ]
      | H -> [ D (G, I, B H); B A ]
      | I -> [ A; C (G, H) ])

let infinite_simple_grammar =
  AbstractGrammar.Grammar.of_productions F infinite_simple_productions

let infinite_simple_member_tree1 = parse_abstract_tree A
let infinite_simple_member_tree2 = parse_abstract_tree (C (A, A))
let infinite_simple_member_tree3 = parse_abstract_tree (B A)

let infinite_simple_member_tree4 =
  parse_abstract_tree (C (A, C (B A, D (B A, A, B (B A)))))

let infinite_simple_member_tree5 = parse_abstract_tree (B (C (A, C (B A, B A))))

let infinite_simple_member_tree6 =
  parse_abstract_tree (D (B A, A, B (D (B (C (A, A)), C (B A, B A), B (B A)))))

let infinite_simple_member_tree7 =
  parse_abstract_tree (B (B (B (B (B (B (B A)))))))

let infinite_simple_nonmember_tree1 = parse_abstract_tree (D (B A, B A, B A))
let infinite_simple_nonmember_tree2 = parse_abstract_tree (C (A, B A))

let infinite_simple_nonmember_tree3 =
  parse_abstract_tree (B (C (A, C (B A, D (B A, A, B A)))))

let infinite_simple_nonmember_tree4 =
  parse_abstract_tree (D (B (B (C (A, B A))), A, B (B A)))

let infinite_loop_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ F; G; H; I; D (C (B F, G), B H, I) ]
      | G -> [ G; H; I; F; C (A, B H) ]
      | H -> [ H; I; F; G; B I ]
      | I -> [ I; F; G; H; A ])

let infinite_loop_grammar =
  AbstractGrammar.Grammar.of_productions F infinite_loop_productions

let infinite_loop_member_tree1 = parse_abstract_tree A
let infinite_loop_member_tree2 = parse_abstract_tree (B A)
let infinite_loop_member_tree3 = parse_abstract_tree (C (A, B A))

let infinite_loop_member_tree4 =
  parse_abstract_tree (D (C (B A, C (A, B A)), B (B A), A))

let infinite_loop_member_tree5 = parse_abstract_tree (B (B (B (B A))))
let infinite_loop_member_tree6 = parse_abstract_tree (C (A, B (C (A, B A))))
let infinite_loop_member_tree7 = parse_abstract_tree (B (C (A, B A)))
let infinite_loop_nonmember_tree1 = parse_abstract_tree (C (A, A))

let infinite_loop_nonmember_tree2 =
  parse_abstract_tree (B (B (B (D (C (B A, A), B A, C (A, A))))))

let infinite_loop_nonmember_tree3 = parse_abstract_tree (C (B A, B A))

let infinite_ambiguous_productions =
  parse_abstract_productions (fun non_term ->
      match non_term with
      | F -> [ B G; H; H; C (I, B A) ]
      | G -> [ C (H, A); I ]
      | H -> [ B (C (F, A)); A; A; H; G; G ]
      | I -> [ F; B A; B (C (C (A, F), G)) ])

let infinite_ambiguous_grammar =
  AbstractGrammar.Grammar.of_productions F infinite_ambiguous_productions

let infinite_ambiguous_member_tree1 = parse_abstract_tree A
let infinite_ambiguous_member_tree2 = parse_abstract_tree (B A)
let infinite_ambiguous_member_tree3 = parse_abstract_tree (B (B (B (B (B A)))))
let infinite_ambiguous_member_tree4 = parse_abstract_tree (B (C (A, A)))

let infinite_ambiguous_member_tree5 =
  parse_abstract_tree (B (C (B (C (A, A)), A)))

let infinite_ambiguous_member_tree6 =
  parse_abstract_tree (B (C (B (C (B (B A), A)), A)))

let infinite_ambiguous_member_tree7 =
  parse_abstract_tree (B (C (B (C (B (B (B A)), A)), A)))

let infinite_ambiguous_member_tree8 = parse_abstract_tree (B (C (C (A, A), A)))
let infinite_ambiguous_nonmember_tree1 = parse_abstract_tree (D (A, A, A))
let infinite_ambiguous_nonmember_tree2 = parse_abstract_tree (C (A, C (A, A)))

let infinite_ambiguous_nonmember_tree3 =
  parse_abstract_tree (C (C (A, C (A, A)), A))

let () =
  let open Alcotest in
  run "Grammar Membership"
    [
      ( "null_grammar",
        [
          assert_not_element "null grammar membership A" single_node_tree
            null_grammar;
          assert_not_element "null grammar membership B" simple_tree
            null_grammar;
        ] );
      ( "empty grammars",
        [
          assert_not_element "empty no loop grammar membership A"
            empty_no_loop_tree1 empty_no_loop_grammar;
          assert_not_element "empty no loop grammar membership B"
            empty_no_loop_tree2 empty_no_loop_grammar;
          assert_not_element "empty symbol loop grammar A"
            empty_symbol_loop_tree1 empty_symbol_loop_grammar;
          assert_not_element "empty symbol loop grammar B"
            empty_symbol_loop_tree2 empty_symbol_loop_grammar;
          assert_not_element "empty symbol loop grammar C"
            empty_symbol_loop_tree3 empty_symbol_loop_grammar;
          assert_not_element "empty symbol loop grammar D"
            empty_symbol_loop_tree4 empty_symbol_loop_grammar;
          assert_not_element "empty loop grammar A" empty_loop_tree1
            empty_loop_grammar;
          assert_not_element "empty loop grammar B" empty_loop_tree2
            empty_loop_grammar;
          assert_not_element "empty loop grammar C" empty_loop_tree3
            empty_loop_grammar;
          assert_not_element "empty loop grammar D" empty_loop_tree4
            empty_loop_grammar;
        ] );
      ( "finite grammars",
        [
          assert_is_element "single direct grammar member" single_member_tree
            single_direct_grammar;
          assert_not_element "single direct grammar non member"
            single_nonmember_tree single_direct_grammar;
          assert_is_element "single derivable grammar member" single_member_tree
            single_derviable_grammar;
          assert_not_element "single derivable grammar non member"
            single_nonmember_tree single_derviable_grammar;
        ] );
      ( "infinite grammars",
        [
          assert_is_element "infinite simple member A"
            infinite_simple_member_tree1 infinite_simple_grammar;
          assert_is_element "infinite simple member B"
            infinite_simple_member_tree2 infinite_simple_grammar;
          assert_is_element "infinite simple member C"
            infinite_simple_member_tree3 infinite_simple_grammar;
          assert_is_element "infinite simple member D"
            infinite_simple_member_tree4 infinite_simple_grammar;
          assert_is_element "infinite simple member E"
            infinite_simple_member_tree5 infinite_simple_grammar;
          assert_is_element "infinite simple member F"
            infinite_simple_member_tree6 infinite_simple_grammar;
          assert_is_element "infinite simple member G"
            infinite_simple_member_tree7 infinite_simple_grammar;
          assert_not_element "infinite simple nonmember A"
            infinite_simple_nonmember_tree1 infinite_simple_grammar;
          assert_not_element "infinite simple nonmember B"
            infinite_simple_nonmember_tree2 infinite_simple_grammar;
          assert_not_element "infinite simple nonmember C"
            infinite_simple_nonmember_tree3 infinite_simple_grammar;
          assert_not_element "infinite simple nonmember D"
            infinite_simple_nonmember_tree4 infinite_simple_grammar;
          assert_is_element "infinite loop member A" infinite_loop_member_tree1
            infinite_loop_grammar;
          assert_is_element "infinite loop member B" infinite_loop_member_tree2
            infinite_loop_grammar;
          assert_is_element "infinite loop member C" infinite_loop_member_tree3
            infinite_loop_grammar;
          assert_is_element "infinite loop member D" infinite_loop_member_tree4
            infinite_loop_grammar;
          assert_is_element "infinite loop member E" infinite_loop_member_tree5
            infinite_loop_grammar;
          assert_is_element "infinite loop member F" infinite_loop_member_tree6
            infinite_loop_grammar;
          assert_is_element "infinite loop member G" infinite_loop_member_tree7
            infinite_loop_grammar;
          assert_not_element "infinite loop nonmember A"
            infinite_loop_nonmember_tree1 infinite_loop_grammar;
          assert_not_element "infinite loop nonmember B"
            infinite_loop_nonmember_tree2 infinite_loop_grammar;
          assert_not_element "infinite loop nonmember C"
            infinite_loop_nonmember_tree3 infinite_loop_grammar;
          assert_is_element "infinite ambiguous member A"
            infinite_ambiguous_member_tree1 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member B"
            infinite_ambiguous_member_tree2 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member C"
            infinite_ambiguous_member_tree3 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member D"
            infinite_ambiguous_member_tree4 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member E"
            infinite_ambiguous_member_tree5 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member F"
            infinite_ambiguous_member_tree6 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member G"
            infinite_ambiguous_member_tree7 infinite_ambiguous_grammar;
          assert_is_element "infinite ambiguous member H"
            infinite_ambiguous_member_tree8 infinite_ambiguous_grammar;
          assert_not_element "infinite ambiguous nonmember A"
            infinite_ambiguous_nonmember_tree1 infinite_ambiguous_grammar;
          assert_not_element "infinite ambiguous nonmember B"
            infinite_ambiguous_nonmember_tree2 infinite_ambiguous_grammar;
          assert_not_element "infinite ambiguous nonmember C"
            infinite_ambiguous_nonmember_tree3 infinite_ambiguous_grammar;
        ] );
    ]
