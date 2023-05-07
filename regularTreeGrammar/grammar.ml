module type GRAMMAR = sig
  type t
  type non_terminal
  type sentence
  type sentential_form

  val of_productions :
    non_terminal -> (non_terminal -> sentential_form list) -> t
  (** [of_productions start_non_terminal productions] creates a new grammar from
      a start non-terminal and the productions for the grammar *)

  val is_element : sentence -> t -> bool
  (** [is_element sentence grammar] determines if the sentence is a member of
      the given grammar *)

  val has_derivation : sentential_form -> t -> bool
  (** [has_derivation sentential_form grammar] determines if the grammar can
      derive the provided sentential form *)
end

(** An module used to construct a regular tree grammar. It constrains the types
    of the sentence and sentential form to share the same set of symbols *)
module type TREE_GRAMMAR_INPUT = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET
  module Tree : Tree.TREE with module RankedAlphabet := RankedAlphabet

  module SentTree :
    SententialTree.SENT_TREE
      with module RankedAlphabet := RankedAlphabet
       and type non_terminal = non_terminal
end

module TreeGrammarImpl =
functor
  (Input : TREE_GRAMMAR_INPUT)
  ->
  struct
    type non_terminal = Input.non_terminal
    type sentence = Input.Tree.t
    type sentential_form = Input.SentTree.t
    type productions = non_terminal -> sentential_form list
    type t = { start : non_terminal; productions : productions }

    module NonTermSentTreeSet = Set.Make (struct
      type t = non_terminal * sentential_form

      let compare = compare
    end)

    let of_productions non_terminal productions =
      { start = non_terminal; productions }

    (** [non_term_has_derivation_rec] is a recursive helper for [non_term_has_derivation]
        which adds the parameter [queried] to track previous calls to the function.
        This allows the function to detect when it enters loops in the grammar
        so that the function always terminates *)
    let rec non_term_has_derivation_rec (non_term : non_terminal)
        (sent_tree : sentential_form) (productions : productions)
        (queried : NonTermSentTreeSet.t) =
      if NonTermSentTreeSet.mem (non_term, sent_tree) queried then false
        (* Encountered a loop in grammar: will not have finite derivation *)
      else
        let new_queried =
          NonTermSentTreeSet.add (non_term, sent_tree) queried
        in
        let sent_trees = productions non_term in
        List.exists
          (fun src_sent_tree ->
            sent_tree_has_derivation src_sent_tree sent_tree productions
              new_queried)
          sent_trees

    (** [has_sent_derivation src_sent_tree dest_sent_tree productions queried]
        is a helper for non_term_has_derivation_rec that determines if the
        src_sent_tree has a derivation to the dest_sent_tree with the given
        productions. The [queried] parameter is just passed back to
        [non_term_has_derivation_rec] *)
    and sent_tree_has_derivation (src_sent_tree : sentential_form)
        (dest_sent_tree : sentential_form) (productions : productions)
        (queried : NonTermSentTreeSet.t) =
      let open Input.SentTree in
      match (destructure src_sent_tree, destructure dest_sent_tree) with
      | Symbol _, NonTerminal _ ->
          (* Would require backwards derivation *)
          false
      | NonTerminal src_non_term, _ ->
          (* Confirm that the non-terminal derives to the destination tree, which may be a symbol or non-terminal *)
          non_term_has_derivation_rec src_non_term dest_sent_tree productions
            queried
      | Symbol (src_symbol, src_children), Symbol (dest_symbol, dest_children)
        ->
          (* Confirm symbols match and children are derivable *)
          src_symbol = dest_symbol
          && List.for_all2
               (fun src_child dest_child ->
                 sent_tree_has_derivation src_child dest_child productions
                   queried)
               src_children dest_children

    (** [non_term_has_derivation] determines if a non-terminal can be derived to
    the given sentential form with the provided productions *)
    let non_term_has_derivation (non_term : non_terminal)
        (sent_tree : sentential_form) (productions : productions) =
      non_term_has_derivation_rec non_term sent_tree productions
        NonTermSentTreeSet.empty

    let has_derivation (sent_tree : sentential_form) (grammar : t) =
      non_term_has_derivation grammar.start sent_tree grammar.productions

    (** [as_sent_tree tree] converts a concrete tree into a sentential tree
        with only symbol nodes, so that we can write functions more
        generally for sentential trees and call them with concrete trees *)
    let rec as_sent_tree (tree : Input.Tree.t) =
      let symbol, children = Input.Tree.destructure tree in
      Input.SentTree.symbol symbol (List.map as_sent_tree children)

    let is_element (tree : sentence) (grammar : t) =
      let sent_tree = as_sent_tree tree in
      has_derivation sent_tree grammar
  end

module type MAKE_FUNCTOR = functor (Input : TREE_GRAMMAR_INPUT) ->
  GRAMMAR
    with type non_terminal = Input.non_terminal
     and type sentential_form = Input.SentTree.t
     and type sentence = Input.Tree.t

module Make : MAKE_FUNCTOR = TreeGrammarImpl
