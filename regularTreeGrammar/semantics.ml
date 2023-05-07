module type SEMANTICS = sig
  type t
  type term
  type eval_rule

  (* TODO: should we make this more abstract and just have an eval_rules type to get rid of the list requriement? *)
  val of_rules : eval_rule list -> t
  (** [of_rules eval_rules] Creates a semantics definition from the list of evaluation rules *)

  val evaluate : t -> term -> term option
  (** [evaluate semantics term] evaluates a term against some semantics, if the
      term can be evaluated under the semantics. This effectively determines the
      if the input term has a corresponding term that forms a pair in the
      evaluation relation *)
end

module SemanticsImpl =
functor
  (EvalRuleMake : EvalRule.MAKE_FUNCTOR)
  (Input : Pattern.REGULAR_PATTERN)
  ->
  struct
    type term = Input.TreeGrammar.Tree.t
    type pattern = Input.Pattern.t
    type eval_relation = pattern * pattern
    type eval_rule = eval_relation list * eval_relation

    (* TODO: consider deconstructing this module to decouple the types *)
    module EvalRule = EvalRuleMake (Input)

    type t = EvalRule.t list

    module TermSet = Set.Make (struct
      type t = term

      let compare = compare
    end)

    let of_rules eval_rules =
      List.map
        (fun eval_rule ->
          let premises, conclusion = eval_rule in
          EvalRule.of_patterns premises conclusion)
        eval_rules

    let rec try_rule (semantics : t) (term : term) (queried : TermSet.t)
        (acc : term option) (eval_rule : EvalRule.t) =
      match acc with
      | Some _ -> acc
      | None ->
          EvalRule.evaluate eval_rule (evaluate_rec semantics queried) term

    and evaluate_rec (semantics : t) (queried : TermSet.t) (term : term) =
      if TermSet.mem term queried then None
        (* Encountered a loop in evaluation: whatever path is being tried cannot have a finite proof tree *)
      else
        let new_queried = TermSet.add term queried in
        List.fold_left (try_rule semantics term new_queried) None semantics

    let evaluate (semantics : t) (term : term) =
      evaluate_rec semantics TermSet.empty term
  end

module type MAKE_FUNCTOR = functor (Input : Pattern.REGULAR_PATTERN) ->
  SEMANTICS
    with type term = Input.TreeGrammar.Tree.t
     and type eval_rule =
      (Input.Pattern.t * Input.Pattern.t) list
      * (Input.Pattern.t * Input.Pattern.t)

module type CUSTOM_MAKE_FUNCTOR = functor (_ : EvalRule.MAKE_FUNCTOR) -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = SemanticsImpl
module Make : MAKE_FUNCTOR = CustomMake (EvalRule.Make)
