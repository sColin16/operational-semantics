open Example.Boolean
open TestHelpers

let assert_evaluates name semantics tree expected =
  let internal_tree = parse_boolean_tree tree in
  let internal_expected = parse_boolean_tree expected in
  let evaluated = BooleanSemantics.Semantics.evaluate semantics internal_tree in
  assert_true name (evaluated = internal_expected)

let small_nested_term : boolean_tree = If (If (True, False, True), True, False)

let medium_nested_term : boolean_tree =
  If
    ( If (True, If (True, False, True), True),
      If (True, False, If (False, False, True)),
      True )

let large_nested_term : boolean_tree =
  If
    ( If
        ( True,
          True,
          If
            ( True,
              If (If (True, False, False), False, If (False, True, False)),
              True ) ),
      If (False, False, False),
      True )

let complete_term_1 : boolean_tree =
  Or
    ( And
        ( And
            ( And
                ( If
                    (False, And (False, True), Or (Not (Or (True, False)), True)),
                  If (And (False, True), False, False) ),
              Or
                ( Not
                    (If
                       ( True,
                         And (And (False, True), And (True, False)),
                         And (Or (True, True), And (False, False)) )),
                  False ) ),
          True ),
      Not True )

(* TODO: we should probably have tests somewhere for validity of semantics *)
(* TODO: we also need tests for sentential form derivations, for stuff like zero derivation non-terminal *)

let () =
  let open Alcotest in
  run "Semantics"
    [
      ( "simple boolean semantics",
        [
          assert_evaluates "true to true" simple_boolean_semantics True True;
          assert_evaluates "false to false" simple_boolean_semantics False False;
          assert_evaluates "if true 1" simple_boolean_semantics
            (If (True, False, True))
            False;
          assert_evaluates "if true 2" simple_boolean_semantics
            (If (True, True, False))
            True;
          assert_evaluates "if false 1" simple_boolean_semantics
            (If (False, False, True))
            True;
          assert_evaluates "if false 2" simple_boolean_semantics
            (If (False, True, False))
            False;
          assert_evaluates "small nested" simple_boolean_semantics
            small_nested_term False;
          assert_evaluates "medium nested" simple_boolean_semantics
            medium_nested_term True;
          assert_evaluates "large nested" simple_boolean_semantics
            large_nested_term False;
        ] );
      ( "complete boolean semantics",
        [
          assert_evaluates "And 1" complete_boolean_semantics
            (And (True, True))
            True;
          assert_evaluates "And 2" complete_boolean_semantics
            (And (True, False))
            False;
          assert_evaluates "And 3" complete_boolean_semantics
            (And (False, True))
            False;
          assert_evaluates "And 4" complete_boolean_semantics
            (And (False, False))
            False;
          assert_evaluates "Or 1" complete_boolean_semantics
            (Or (True, True))
            True;
          assert_evaluates "Or 2" complete_boolean_semantics
            (Or (True, False))
            True;
          assert_evaluates "Or 3" complete_boolean_semantics
            (Or (False, True))
            True;
          assert_evaluates "Or 4" complete_boolean_semantics
            (Or (False, False))
            False;
          assert_evaluates "Not true" complete_boolean_semantics (Not True)
            False;
          assert_evaluates "Not false" complete_boolean_semantics (Not False)
            True;
          assert_evaluates "Nested and" complete_boolean_semantics
            (And (And (False, True), And (True, False)))
            False;
          assert_evaluates "Nested or" complete_boolean_semantics
            (Or (Or (True, False), Or (True, True)))
            True;
          assert_evaluates "Nested not" complete_boolean_semantics
            (Not (Not (Not (Not True)))) True;
          assert_evaluates "Large program" complete_boolean_semantics
            complete_term_1 False;
        ] );
    ]
