module type PATTERN = sig
  type t
  type obj
  type assignments
  type input

  val create : input -> t
  (** Creates a pattern if the pattern is considered valid. May raise Invalid_argument *)

  val unwrap : t -> input
  (** The inverse operation of create, returns the underlying pattern input type *)

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

(* TODO: do the assignment need to be concrete? Or can we get away keeping them abstract? *)
(* If I need to make them concrete, it's actually pretty simple *)
module type REG_TREE_PATTERN_INPUT = sig
  module RankedAlphabet : Common.RANKED_ALPHABET

  type non_terminal

  (* I like the TREE_GRAMMAR type here because it reduces the number of type constraints I need to type here.
     It does possible introduce additional type constraints that may not be necessary, but I think worrying
     about that is kinda like worrying about passing functions instead of modules: it adds additional complexity
     and more tightly couple the input here to the implementation below *)
  module TreeGrammar :
    TreeGrammar.TREE_GRAMMAR
      with module RankedAlphabet := RankedAlphabet
       and type non_terminal := non_terminal

  module PatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with module RankedAlphabet := RankedAlphabet
       and type non_terminal := non_terminal

  module GrammarCollection :
    GrammarCollection.GRAMMAR_COLLECTION
      with type grammar = TreeGrammar.Grammar.t
       and type non_terminal = non_terminal

  module TreePatternAssignments :
    TreePatternAssignments.GRAMMAR_PATTERN_ASSIGNMENTS
      with type sentence = TreeGrammar.Tree.t
       and module GrammarCollection = GrammarCollection
end

module TreePatternImpl =
functor
  (ValidTreePatternMake : SetElement.MAKE_PATTERN_TREE_FUNCTOR)
  (Input : REG_TREE_PATTERN_INPUT)
  ->
  struct
    module ValidPatternTree = ValidTreePatternMake (struct
      type non_terminal = Input.non_terminal

      module RankedAlphabet = Input.RankedAlphabet
      module TreeGrammar = Input.TreeGrammar
      module PatternTree = Input.PatternTree
    end)

    type obj = Input.TreeGrammar.Tree.t
    type assignments = Input.TreePatternAssignments.t
    type input = Input.PatternTree.t
    type t = ValidPatternTree.t

    let rec match_on_rec (pattern : Input.PatternTree.t) (tree : obj) =
      match
        ( Input.PatternTree.destructure pattern,
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

    let rec substitute_with_rec (pattern : Input.PatternTree.t)
        (assignments : assignments) =
      match Input.PatternTree.destructure pattern with
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

    let create (pattern_tree : input) =
      let primary_grammar =
        Input.GrammarCollection.mapping Input.GrammarCollection.start_non_terminal
      in
      match ValidPatternTree.create_opt pattern_tree primary_grammar with
      | Some valid_tree -> valid_tree
      | None ->
          raise
            (Invalid_argument
               "The pattern tree was not an element of the primary grammar")

    let unwrap pattern_tree = ValidPatternTree.unwrap pattern_tree

    let match_on (pattern : t) (tree : obj) =
      match_on_rec (ValidPatternTree.unwrap pattern) tree

    let substitute_with (pattern : t) (assignments : assignments) =
      substitute_with_rec (ValidPatternTree.unwrap pattern) assignments
  end

module type MAKE_FUNCTOR = functor (Input : REG_TREE_PATTERN_INPUT) ->
  PATTERN
    with type input = Input.PatternTree.t
     and type obj = Input.TreeGrammar.Tree.t
     and type assignments = Input.TreePatternAssignments.t

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : SetElement.MAKE_PATTERN_TREE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = TreePatternImpl
module Make : MAKE_FUNCTOR = TreePatternImpl (SetElement.MakePatternTree)
