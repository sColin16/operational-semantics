(* A few helper test function because Alcotest is unecessarily verbose for most use cases *)
let assert_bool name expected actual =
  let test_function () = Alcotest.(check bool) name expected actual in
  Alcotest.test_case name `Quick test_function

let assert_true name value = assert_bool name true value
let assert_false name value = assert_bool name false value

type test_symbols = True | False | If
type test_non_terminals = T | V | A | B
type boolean_symbols = True | False | Not | And | Or | If
type boolean_non_terminals = T | V
type arithmetic_symbols = Zero | Succ | Plus
type arithmetic_non_terminals = T | V



let test_ranked_alphabet (symbol : test_symbols) =
  match symbol with True -> 0 | False -> 0 | If -> 3

let boolean_ranked_alphabet (symbol : boolean_symbols) =
  match symbol with
  | True -> 0
  | False -> 0
  | Not -> 1
  | And -> 2
  | Or -> 2
  | If -> 3

let arithmetic_ranked_alphabet (symbol : arithmetic_symbols) =
  match symbol with Zero -> 0 | Succ -> 1 | Plus -> 2

module TestGrammar = Regular_tree_language.RegTreeGrammar (struct
  type symbol = test_symbols
  type non_terminal = test_non_terminals

  let ranked_alphabet = test_ranked_alphabet
end)

module BooleanGrammar = Regular_tree_language.RegTreeGrammar (struct
  type symbol = boolean_symbols
  type non_terminal = boolean_non_terminals

  let ranked_alphabet = boolean_ranked_alphabet
end)

module ArithmeticGrammar = Regular_tree_language.RegTreeGrammar (struct
  type symbol = arithmetic_symbols
  type non_terminal = arithmetic_non_terminals

  let ranked_alphabet = arithmetic_ranked_alphabet
end)

let test_productions (symbol : test_non_terminals) : TestGrammar.sent_tree list
    =
  match symbol with
  | T ->
      [
        `Symbol (True, []);
        `Symbol (False, []);
        `Symbol (If, [ `NonTerminal T; `NonTerminal T; `NonTerminal T ]);
      ]
  | V -> [ `Symbol (True, []); `Symbol (False, []) ]
  | _ -> []

let boolean_productions (symbol : boolean_non_terminals) :
    BooleanGrammar.sent_tree list =
  match symbol with
  | T ->
      [
        `Symbol (True, []);
        `Symbol (False, []);
        `Symbol (Not, [ `NonTerminal T ]);
        `Symbol (And, [ `NonTerminal T; `NonTerminal T ]);
        `Symbol (Or, [ `NonTerminal T; `NonTerminal T ]);
        `Symbol (If, [ `NonTerminal T; `NonTerminal T; `NonTerminal T ]);
      ]
  | V -> [ `Symbol (True, []); `Symbol (False, []) ]

let arithmetic_productions symbol =
  match symbol with
  | T ->
      [
        `Symbol (Zero, []);
        `Symbol (Succ, [ `NonTerminal T ]);
        `Sybmol (Plus, [ `NonTerminal T; `NonTerminal T ]);
      ]
  | V -> [ `Symbol (Zero, []); `Symbol (Succ, [ `NonTerminal V ]) ]
