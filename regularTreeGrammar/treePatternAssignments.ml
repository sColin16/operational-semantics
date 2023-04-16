module type GRAMMAR_PATTERN_ASSIGNMENTS = sig
  type t
  type non_terminal
  type grammar
  type sentence
  type key = non_terminal * int
  type grammar_collection_t = non_terminal -> grammar

  (* I have decided to include this as a value in the module, as it is part of the semantics. You must be using the same
     grammar collection ot merge assignments, and including this in the module enforces that at the type level *)

  val grammar_collection : grammar_collection_t
  (** A mapping definig the grammar for each non-terminal. Usually, these all share the same productions *)

  val empty : t
  (** An empty assignment mapping *)

  val singleton_opt : key -> sentence -> t option
  (** [singleton_opt key sentence] returns a singleton assignment mapping, if
      the sentence is assignable to the given key *)

  val find_opt : key -> t -> sentence option
  (** [find_opt key assignment] returns the corresponding assignment for the
      key, if it exists *)

  val merge_assignments : t option list -> t option
  (** Merges a list of assignments into a single assignment, if a valid assignment exists *)
end

module type TREE_ASSIGNMENT_INPUT = sig
  module TreeGrammar : Grammar.GRAMMAR

  val grammar_collection : TreeGrammar.non_terminal -> TreeGrammar.t
end

module AssignmentsImpl =
functor
  (GrammarElementMake : SetElement.MAKE_SENTENCE_FUNCTOR)
  (Input : TREE_ASSIGNMENT_INPUT)
  ->
  struct
    type non_terminal = Input.TreeGrammar.non_terminal
    type grammar = Input.TreeGrammar.t
    type sentence = Input.TreeGrammar.sentence
    type key = non_terminal * int
    type grammar_collection_t = non_terminal -> grammar

    module GrammarElement = GrammarElementMake (Input.TreeGrammar)

    module AssignmentMap = Map.Make (struct
      type t = key

      let compare = compare
    end)

    type t = GrammarElement.t AssignmentMap.t

    let grammar_collection = Input.grammar_collection
    let empty = AssignmentMap.empty

    let singleton_opt key tree =
      let non_term, index = key in
      match GrammarElement.create_opt tree (grammar_collection non_term) with
      | Some wrapped_elt ->
          Some (AssignmentMap.singleton (non_term, index) wrapped_elt)
      | None -> None

    let rec find_opt key assignments =
      match AssignmentMap.find_opt key assignments with
      | Some wrapped_elt -> Some (GrammarElement.unwrap wrapped_elt)
      | None -> None

    let merge_assignments_noopt (assignments_a : t) (assignments_b : t) =
      let merge_function _ val1 val2 =
        assert (val1 = val2);
        Some val1
      in
      try Some (AssignmentMap.union merge_function assignments_a assignments_b)
      with Assert_failure _ -> None

    let merge_assignments_opt assignments_a assignments_b =
      match (assignments_a, assignments_b) with
      | None, _ | _, None -> None
      | Some a, Some b -> merge_assignments_noopt a b

    let merge_assignments assignments_list =
      List.fold_left merge_assignments_opt (Some AssignmentMap.empty)
        assignments_list
  end

module type MAKE_FUNCTOR = functor (Input : TREE_ASSIGNMENT_INPUT) ->
  GRAMMAR_PATTERN_ASSIGNMENTS
    with type non_terminal = Input.TreeGrammar.non_terminal
     and type grammar = Input.TreeGrammar.t
     and type sentence = Input.TreeGrammar.sentence

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : SetElement.MAKE_SENTENCE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = AssignmentsImpl
module Make : MAKE_FUNCTOR = AssignmentsImpl (SetElement.MakeSentence)
