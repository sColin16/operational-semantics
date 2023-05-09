module type SEMANTICS = sig
  type t
  type term
  type eval_rule

  (* TODO: should we make this more abstract and just have an eval_rules type to get rid of the list requriement? *)
  val of_rules : eval_rule list -> t
  (** [of_rules eval_rules] Creates a semantics definition from the list of evaluation rules *)

  (* TODO: update the comments here *)
  val step : t -> term -> term option
  (** [step semantics term] evaluates a term against some semantics, if the
      term can be evaluated under the semantics. This effectively determines the
      if the input term has a corresponding term that forms a pair in the
      evaluation relation *)

  val evaluate : t -> term -> term
end

module type SEMANTICS_INPUT = sig
  (* Required for internal creation of Pattern module *)
  module RankedAlphabet : Common.RANKED_ALPHABET

  (* Required type constraint for PatternTrees for EvalRule *)
  type non_terminal

  (* Required for external type *)
  module PatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with type non_terminal = non_terminal
       and module RankedAlphabet = RankedAlphabet

  (* Required for the internal creation of a pattern, grammar collection constraint *)
  module TreeGrammar :
    TreeGrammar.TREE_GRAMMAR
      with type non_terminal = non_terminal
      with module RankedAlphabet = RankedAlphabet

  (* Required for constraint on the internal pattern *)
  (* I'm not a fan of this grammar collection being part of this type. Not sure how to avoid it though... *)
  module GrammarCollection :
    GrammarCollection.GRAMMAR_COLLECTION
      with type grammar = TreeGrammar.Grammar.t
       and type non_terminal = non_terminal
end

module SemanticsImpl =
functor
  (EvalRuleMake : EvalRule.MAKE_FUNCTOR)
  (PatternMake : Pattern.MAKE_FUNCTOR)
  (TreePatternAssignmentsMake : TreePatternAssignments.MAKE_FUNCTOR)
  (Input : SEMANTICS_INPUT)
  ->
  struct
    type term = Input.TreeGrammar.Tree.t

    type eval_rule =
      (Input.PatternTree.t * Input.PatternTree.t) list
      * (Input.PatternTree.t * Input.PatternTree.t)

    module TreePatternAssignments = TreePatternAssignmentsMake (struct
      type non_terminal = Input.non_terminal

      module Grammar = Input.TreeGrammar.Grammar
      module GrammarCollection = Input.GrammarCollection
    end)

    module Pattern = PatternMake (struct
      module RankedAlphabet = Input.RankedAlphabet

      type non_terminal = Input.non_terminal

      module TreeGrammar = Input.TreeGrammar
      module PatternTree = Input.PatternTree
      module GrammarCollection = Input.GrammarCollection
      module TreePatternAssignments = TreePatternAssignments
    end)

    module EvalRule = EvalRuleMake (struct
      type term = Input.TreeGrammar.Tree.t
      type non_terminal = Input.non_terminal

      module PatternTree = Input.PatternTree
      module TreePatternAssignments = TreePatternAssignments
      module Pattern = Pattern
    end)

    type t = EvalRule.t list

    module TermSet = Set.Make (struct
      type t = term

      let compare = compare
    end)

    let verify_eval_relation
        (eval_relation : Input.PatternTree.t * Input.PatternTree.t) =
      let left, right = eval_relation in
      (Pattern.create left, Pattern.create right)

    let verify_eval_rule (eval_rule : eval_rule) =
      let premises, conclusion = eval_rule in
      EvalRule.of_rule
        (List.map verify_eval_relation premises)
        (verify_eval_relation conclusion)

    let of_rules (eval_rules : eval_rule list) =
      List.map verify_eval_rule eval_rules

    let rec try_rule (semantics : t) (term : term) (queried : TermSet.t)
        (acc : term option) (eval_rule : EvalRule.t) =
      match acc with
      | Some _ -> acc
      | None ->
          EvalRule.step eval_rule (step_rec semantics queried) term

    and step_rec (semantics : t) (queried : TermSet.t) (term : term) =
      if TermSet.mem term queried then None
        (* Encountered a loop in evaluation: whatever path is being tried cannot have a finite proof tree *)
      else
        let new_queried = TermSet.add term queried in
        List.fold_left (try_rule semantics term new_queried) None semantics

    let step (semantics : t) (term : term) =
      step_rec semantics TermSet.empty term

    let rec evaluate (semantics : t) (term : term) =
      let evaluated = step semantics term in
      match evaluated with
      | None -> term
      | Some next_term -> evaluate semantics next_term
  end

module type MAKE_FUNCTOR = functor (Input : SEMANTICS_INPUT) ->
  SEMANTICS
    with type term = Input.TreeGrammar.Tree.t
     and type eval_rule =
      (Input.PatternTree.t * Input.PatternTree.t) list
      * (Input.PatternTree.t * Input.PatternTree.t)

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : EvalRule.MAKE_FUNCTOR)
  (_ : Pattern.MAKE_FUNCTOR)
  (_ : TreePatternAssignments.MAKE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = SemanticsImpl

module Make : MAKE_FUNCTOR =
  CustomMake (EvalRule.Make) (Pattern.Make) (TreePatternAssignments.Make)
