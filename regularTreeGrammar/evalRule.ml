module type EVAL_RULE = sig
  type t
  type term
  type eval_relation

  val of_rule : eval_relation list -> eval_relation -> t
  (** [of_rule premises conclusion] creates an evaluation rule from the
      components of an inference rule: the given premises and conclusion. May
      throw Invalid_argument if the premises or conclusion do not form a valid
      evaluation rule *)

  val evaluate : t -> (term -> term option) -> term -> term option
  (** [evaluate eval_rule global_eval term] evaluates the term against the evaluation rule,
      in the context of a given global_eval function, which is often a SEMANTICS
      evaluation function *)
end

module type EVAL_RULE_INPUT = sig
  (* Required for external type *)
  type term

  (* Required for internal rule verifier *)
  type non_terminal

  (* Required for internal rule verifier *)
  module PatternTree :
    PatternTree.REGULAR_PATTERN_TREE with type non_terminal = non_terminal

  (* Required as type constraint for pattern to merge assignments *)
  module TreePatternAssignments :
    TreePatternAssignments.GRAMMAR_PATTERN_ASSIGNMENTS

  (* Required for external type *)
  module Pattern :
    Pattern.PATTERN
      with type obj = term
       and type assignments = TreePatternAssignments.t
       and type input = PatternTree.t
end

module EvalRuleImpl =
functor
  (RuleVerifierMake : EvalRuleVerifier.MAKE_FUNCTOR)
  (Input : EVAL_RULE_INPUT)
  ->
  struct
    type term = Input.term
    type eval_relation = Input.Pattern.t * Input.Pattern.t

    type t = {
      ordered_premises : eval_relation list;
      conclusion : eval_relation;
    }

    module RuleVerifier = RuleVerifierMake (struct
      type non_terminal = Input.non_terminal

      module PatternTree = Input.PatternTree
      module Pattern = Input.Pattern
    end)

    let of_rule premises conclusion =
      let ordered_premises = RuleVerifier.order_premises premises conclusion in
      match ordered_premises with
      | Some valid_ordered_premises ->
          { ordered_premises = valid_ordered_premises; conclusion }
      (* TODO: Enhancement is to detect which premises can't be checked and metavariables preventing that *)
      | None ->
          raise
            (Invalid_argument
               "Evaluation rule integrity check failed. Double-check that your \
                rule is correct")

    let resolve_premise (global_eval : term -> term option)
        (assignments_opt : Input.Pattern.assignments option)
        (premise : eval_relation) =
      match assignments_opt with
      | None -> None
      | Some assignments -> (
          let premise_left, premise_right = premise in
          let substituted_term =
            Input.Pattern.substitute_with premise_left assignments
          in
          match global_eval substituted_term with
          | None -> None
          | Some evaluated_term ->
              let new_assignments =
                Input.Pattern.match_on premise_right evaluated_term
              in
              Input.TreePatternAssignments.merge_assignments
                [ assignments_opt; new_assignments ])

    let evaluate eval_rule global_eval term =
      let conc_left, conc_right = eval_rule.conclusion in
      match Input.Pattern.match_on conc_left term with
      | None -> None
      | Some init_assignments -> (
          let premise_assignments =
            List.fold_left
              (resolve_premise global_eval)
              (Some init_assignments) eval_rule.ordered_premises
          in
          match premise_assignments with
          | None -> None
          | Some final_assigments ->
              Some (Input.Pattern.substitute_with conc_right final_assigments))
  end

module type MAKE_FUNCTOR = functor (Input : EVAL_RULE_INPUT) ->
  EVAL_RULE
    with type term = Input.term
     and type eval_relation = Input.Pattern.t * Input.Pattern.t

module type CUSTOM_MAKE_FUNCTOR = functor (_ : EvalRuleVerifier.MAKE_FUNCTOR) ->
  MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = EvalRuleImpl
module Make : MAKE_FUNCTOR = CustomMake (EvalRuleVerifier.Make)
