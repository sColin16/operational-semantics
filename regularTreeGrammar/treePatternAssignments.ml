module type GRAMMAR_PATTERN_ASSIGNMENTS = sig
  type t
  type sentence

  (* TODO: should the type be extracted from the collection into this type? Substituted out? *)
  (* I have decided to include this as a value in the module, as it is part of the semantics. You must be using the same
     grammar collection to merge assignments, and including this in the module enforces that at the type level *)
  module GrammarCollection : GrammarCollection.GRAMMAR_COLLECTION

  val empty : t
  (** An empty assignment mapping *)

  val singleton_opt :
    GrammarCollection.non_terminal * int -> sentence -> t option
  (** [singleton_opt key sentence] returns a singleton assignment mapping, if
      the sentence is assignable to the given key *)

  val find_opt : GrammarCollection.non_terminal * int -> t -> sentence option
  (** [find_opt key assignment] returns the corresponding assignment for the
      key, if it exists *)

  val merge_assignments : t option list -> t option
  (** Merges a list of assignments into a single assignment, if a valid assignment exists *)
end

module type TREE_ASSIGNMENT_INPUT = sig
  type non_terminal

  module Grammar : Grammar.GRAMMAR with type non_terminal = non_terminal

  module GrammarCollection :
    GrammarCollection.GRAMMAR_COLLECTION
      with type grammar = Grammar.t
       and type non_terminal = non_terminal
end

module AssignmentsImpl =
functor
  (GrammarElementMake : SetElement.MAKE_SENTENCE_FUNCTOR)
  (Input : TREE_ASSIGNMENT_INPUT)
  ->
  struct
    type sentence = Input.Grammar.sentence

    module GrammarCollection = Input.GrammarCollection
    module GrammarElement = GrammarElementMake (Input.Grammar)

    module AssignmentMap = Map.Make (struct
      type t = Input.GrammarCollection.non_terminal * int

      let compare = compare
    end)

    type t = GrammarElement.t AssignmentMap.t

    let empty = AssignmentMap.empty

    let singleton_opt key tree =
      let non_term, index = key in
      match GrammarElement.create_opt tree (Input.GrammarCollection.mapping non_term) with
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
    with type sentence = Input.Grammar.sentence
    and module GrammarCollection = Input.GrammarCollection

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : SetElement.MAKE_SENTENCE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = AssignmentsImpl
module Make : MAKE_FUNCTOR = AssignmentsImpl (SetElement.MakeSentence)
