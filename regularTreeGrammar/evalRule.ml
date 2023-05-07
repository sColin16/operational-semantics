module type EVAL_RULE = sig
  type t
  type term
  type pattern
  type eval_relation = pattern * pattern

  val of_patterns : eval_relation list -> eval_relation -> t
  (** [of_patterns premises conclusion] creates an evaluation rule (an inference rule)
      from the given premises and conclusion. May throw Invalid_argument if the premises
      or conclusion do not form a valid evaluation rule *)

  val evaluate : t -> (term -> term option) -> term -> term option
  (** [evaluate eval_rule global_eval term] evaluates the term against the evaluation rule,
      in the context of a given global_eval function, which is often a SEMANTICS
      evaluation function *)
end

module EvalRuleImpl =
functor
  (RuleVerifierMake : EvalRuleVerifier.MAKE_FUNCTOR)
  (Input : Pattern.REGULAR_PATTERN)
  ->
  struct
    type term = Input.TreeGrammar.Tree.t
    type pattern = Input.Pattern.t
    type eval_relation = pattern * pattern

    type t = {
      ordered_premises : eval_relation list;
      conclusion : eval_relation;
    }

    (* TODO: It would probably be good to destructur this module just to decouple the input types *)
    module RuleVerifier = RuleVerifierMake (Input)

    let of_patterns premises conclusion =
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

module type MAKE_FUNCTOR = functor (Input : Pattern.REGULAR_PATTERN) ->
  EVAL_RULE
    with type term = Input.TreeGrammar.Tree.t
     and type pattern = Input.Pattern.t

module type CUSTOM_MAKE_FUNCTOR = functor (_ : EvalRuleVerifier.MAKE_FUNCTOR) ->
  MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = EvalRuleImpl
module Make : MAKE_FUNCTOR = CustomMake (EvalRuleVerifier.Make)
