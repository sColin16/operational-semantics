module type SEMANTICS = sig
  type t
  type term
  type eval_rule

  val of_rules : eval_rule list -> t
  (** [of_rules eval_rules] Creates a semantics definition from the list of evaluation rules *)

  val evaluate : t -> term -> term option
  (** [evaluate semantics term] evaluates a term against some semantics, if the
      term can be evaluated under the semantics. This effectively determines the
      if the input term has a corresponding term that forms a pair in the
      evaluation relation *)
end

module type INPUT = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET

  module RegularPattern :
    Pattern.REGULAR_PATTERN
      with type non_terminal = non_terminal
       and module RankedAlphabet = RankedAlphabet

  module EvalRule :
    EvalRule.EVAL_RULE
      with type term = RegularPattern.TreeGrammar.Tree.t
       and type pattern = RegularPattern.Pattern.t
end

module SemanticsImpl =
functor
  (Input : INPUT)
  ->
  struct
    type term = Input.RegularPattern.TreeGrammar.Tree.t
    type eval_rule = Input.EvalRule.t
    type t = eval_rule list

    module TermSet = Set.Make (struct
      type t = term

      let compare = compare
    end)

    let of_rules eval_rules = eval_rules

    let rec try_rule (semantics : t) (term : term) (queried : TermSet.t)
        (acc : term option) (eval_rule : eval_rule) =
      match acc with
      | Some _ -> acc
      | None ->
          Input.EvalRule.evaluate eval_rule
            (evaluate_rec semantics queried)
            term

    and evaluate_rec (semantics : t) (queried : TermSet.t) (term : term) =
      if TermSet.mem term queried then None
        (* Encountered a loop in evaluation: whatever path is being tried cannot have a finite proof tree *)
      else
        let new_queried = TermSet.add term queried in
        List.fold_left (try_rule semantics term new_queried) None semantics

    let evaluate (semantics : t) (term : term) =
      evaluate_rec semantics TermSet.empty term
  end

module type MAKE_FUNCTOR = functor (Input : INPUT) ->
  SEMANTICS
    with type term = Input.RegularPattern.TreeGrammar.Tree.t
     and type eval_rule = Input.EvalRule.t

module Make : MAKE_FUNCTOR = SemanticsImpl
