module type PATTERN = sig
  type t
  type obj
  type assignments
  type input

  val create : input -> t
  (** Creates a pattern if the pattern is considered valid. May throw Invalid_argument *)

  val match_on : t -> obj -> assignments option
  (** Matches an object again a pattern, extracting the assignments if the
      object matches *)

  val substitute_with : t -> assignments -> obj
  (** Substitutes assignments into a pattern to create an object: the inverse
      operation of match_on *)

  (* TODO: seriously consider adding these *)
  (* substitute_with_opt: t -> assignments -> obj option *)
  (* Accepts an incomplete set of assignments that may simply produce another pattern *)
  (* val partial_substitute : t -> assignments -> t *)
  (* Creates a pattern that contains no metavariables*)
  (* val as_pattern : obj -> t *)
  (* Converts a pattern to an object, if the pattern contains no metavariables *)
  (* val as_obj : t-> obj option *)

  (* For cases where you can substitute or match on patterns themselves, I think you should have
     a different instance of the pattern to handle that. You could combine those into a larger module if deisred *)
end

module type REG_TREE_PATTERN_INPUT = sig
  module RankedAlphabet : Common.RANKED_ALPHABET

  type non_terminal

  module RegularPatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with module RankedAlphabet := RankedAlphabet
       and type non_terminal := non_terminal

  module TreeGrammar :
    Grammar.TREE_GRAMMAR
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet

  module TreePatternAssignments :
    TreePatternAssignments.GRAMMAR_PATTERN_ASSIGNMENTS
      with type non_terminal := non_terminal
       and type sentence := TreeGrammar.Tree.t
       and type grammar := TreeGrammar.Grammar.t
end

module FreeTreePatternImpl =
functor
  (Input : REG_TREE_PATTERN_INPUT)
  ->
  struct
    type obj = Input.TreeGrammar.Tree.t
    type non_terminal = Input.non_terminal
    type assignments = Input.TreePatternAssignments.t
    type input = Input.RegularPatternTree.t
    type t = input

    let rec match_on_rec (pattern : Input.RegularPatternTree.t) (tree : obj) =
      match
        ( Input.RegularPatternTree.destructure pattern,
          Input.TreeGrammar.Tree.destructure tree )
      with
      | Symbol (pattern_symbol, pattern_children), (tree_symbol, tree_children)
        ->
          if pattern_symbol = tree_symbol then
            Input.TreePatternAssignments.merge_assignments
              (List.map2 match_on_rec pattern_children tree_children)
          else None
      | NonTerminal (pattern_non_term, pattern_index), _ ->
          Input.TreePatternAssignments.singleton_opt
            (pattern_non_term, pattern_index)
            tree

    let rec substitute_with_rec (pattern : Input.RegularPatternTree.t)
        (assignments : assignments) =
      match Input.RegularPatternTree.destructure pattern with
      | Symbol (symbol, children) ->
          Input.TreeGrammar.Tree.node symbol
            (List.map
               (fun child_pattern ->
                 substitute_with_rec child_pattern assignments)
               children)
      | NonTerminal (non_term, index) -> (
          match
            Input.TreePatternAssignments.find_opt (non_term, index) assignments
          with
          | None -> raise (Invalid_argument "assignment was incomplete")
          | Some tree -> tree)

    let create (pattern_tree : input) = pattern_tree
    let match_on (pattern : t) (tree : obj) = match_on_rec pattern tree

    let substitute_with (pattern : t) (assignments : assignments) =
      substitute_with_rec pattern assignments
  end

module type FREE_MAKE_FUNCTOR = functor (Input : REG_TREE_PATTERN_INPUT) ->
  PATTERN
    with type input = Input.RegularPatternTree.t
     and type obj = Input.TreeGrammar.Tree.t
     and type assignments = Input.TreePatternAssignments.t

module FreeMake : FREE_MAKE_FUNCTOR = FreeTreePatternImpl

module type CONSTRAINED_TREE_PATTERN_INPUT = sig
  module RankedAlphabet : Common.RANKED_ALPHABET

  type non_terminal

  module RegularPatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with module RankedAlphabet := RankedAlphabet
       and type non_terminal := non_terminal

  module TreeGrammar :
    Grammar.TREE_GRAMMAR
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet

  module TreePatternAssignments :
    TreePatternAssignments.GRAMMAR_PATTERN_ASSIGNMENTS
      with type non_terminal := non_terminal
       and type sentence := TreeGrammar.Tree.t
       and type grammar := TreeGrammar.Grammar.t

  module ValidPatternTree :
    SetElement.SET_ELEMENT
      with type elt = RegularPatternTree.t
       and type set = TreeGrammar.Grammar.t
end

module ConstrainedPatternTreeImpl =
functor
  (FreeTreePatternMake : FREE_MAKE_FUNCTOR)
  (Input : CONSTRAINED_TREE_PATTERN_INPUT)
  ->
  struct
    module FreeTreePattern = FreeTreePatternMake (Input)

    type t = Input.ValidPatternTree.t
    type obj = Input.TreeGrammar.Tree.t
    type assignments = Input.TreePatternAssignments.t
    type input = Input.ValidPatternTree.t
    type non_terminal = Input.non_terminal

    let to_free_pattern pattern =
      FreeTreePattern.create (Input.ValidPatternTree.unwrap pattern)

    let create pattern_tree = pattern_tree

    let match_on pattern tree =
      FreeTreePattern.match_on (to_free_pattern pattern) tree

    let substitute_with pattern assignments =
      FreeTreePattern.substitute_with (to_free_pattern pattern) assignments
  end

  module type MAKE_FUNCTOR = functor (Input : CONSTRAINED_TREE_PATTERN_INPUT) ->
    PATTERN
      with type input = Input.ValidPatternTree.t
       and type obj = Input.TreeGrammar.Tree.t
       and type assignments = Input.TreePatternAssignments.t

  module type CUSTOM_MAKE_FUNCTOR = functor (_ : FREE_MAKE_FUNCTOR) -> MAKE_FUNCTOR

  module CustomMake : CUSTOM_MAKE_FUNCTOR = ConstrainedPatternTreeImpl
  module Make : MAKE_FUNCTOR = CustomMake (FreeMake)
