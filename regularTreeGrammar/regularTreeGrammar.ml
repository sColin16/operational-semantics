module type REG_TREE_ALPHABET = sig
  type symbol
  type non_terminal

  val ranked_alphabet : symbol -> int
end

(* TODO: can we make symbol and non-terminal polymorphic types? Can we still use Set/Map packages that way? *)
module type REG_TREE_GRAMMAR = sig
  type symbol
  type non_terminal
  type tree = [ `Symbol of symbol * tree list ]

  type sent_tree =
    [ `Symbol of symbol * sent_tree list | `NonTerminal of non_terminal ]

  type grammar
  type sent_tree_set

  (* TODO: give the ability to attach the non-terminal with invalid productions to this *)
  exception Invalid_production of string
  exception Invalid_tree of string

  val create_grammar :
    non_terminal -> (non_terminal -> sent_tree list) -> grammar
  (** [create_grammar start_non_term productions] creates a grammar with the given productions and the given start non terminal *)

  val tree_in_alphabet : tree -> bool
  (** [tree_in_alphabet tree] Determines if the provided tree is valid with
  regard to the ranked alphabet for the grammar *)

  val sent_tree_in_alphabet : sent_tree -> bool
  val is_element : tree -> grammar -> bool
  val is_sent_element : sent_tree -> grammar -> bool
end

(* TODO: should this be called Make like the map and set equivalents *)
(* TODO: should we have submodules for creating trees and sent_trees *)
(* module RegTreeGrammarFunctor(Alphabet : RegTreeAlphabet) : (RegTreeGrammarType with type symbol = Alphabet.symbol and type non_terminal = Alphabet.non_terminal) = *)
module RegTreeGrammarImplementation =
functor
  (Alphabet : REG_TREE_ALPHABET)
  ->
  struct
    type symbol = Alphabet.symbol
    type non_terminal = Alphabet.non_terminal
    type tree = [ `Symbol of Alphabet.symbol * tree list ]

    type sent_tree =
      [ `Symbol of Alphabet.symbol * sent_tree list
      | `NonTerminal of Alphabet.non_terminal ]

    (** I chose to have productions be functinos mapping from non-terminals to lists of sent_trees since
      * I believe that is the most syntactically straightforward way to define them
      * Internally, I can really represent them however I want: maps instead of functions, sets instead of lists, etc. *)

    (** Benefits of using types instead of sets of values:
      * You get type-checking for symbols instead of needing to verify them at runtime
      * Type-based merging of non-terminals, instead of doing things with strings
      * No arbitrary choice of strings
      * No need to have the node/leaf wrapper around symbol/non-terminal since types enforce no overlap of labels
     * Downsides of using types
      * You can have an infinite-sized alphabet, which is kinda strange. Also, the symbols can have parameters
      * We can't manipulate the set of symbols and non-terminals at runtime, identifying and removing useless ones, for example
      * Possibly overly restrictive type safety that makes it difficult to construct new non-terminals at runtime?
      * No ability to use a subset of a larger type, like strings (unless you do something weird like make the ranked alphabet have arity of -1 for all other symbols)
      * No readily available list of non-terminals, must either have them passed or compute reachable ones from the productions
    *)
    module SentTreeSet = Set.Make (struct
      type t = sent_tree

      let compare = compare
    end)

    module NonTermSet = Set.Make (struct
      type t = non_terminal

      let compare = compare
    end)

    type a = NonTermSet.elt

    module NonTermSentTreeSet = Set.Make (struct
      type t = non_terminal * sent_tree

      let compare = compare
    end)

    type sent_tree_set = SentTreeSet.t
    type productions = non_terminal -> SentTreeSet.t
    type grammar = { start : non_terminal; productions : productions }

    exception Invalid_production of string
    exception Invalid_tree of string

    let rec sent_tree_in_alphabet (tree : sent_tree) =
      match tree with
      | `NonTerminal _ -> true
      | `Symbol (sym, children) ->
          List.length children = Alphabet.ranked_alphabet sym
          && List.for_all sent_tree_in_alphabet children

    let tree_in_alphabet (tree : tree) =
      sent_tree_in_alphabet (tree :> sent_tree)

    (* The queried argument tracks which instances of the function were called
       as part of the search. If we find a duplicate, then we have entered a
       logical loop where the sent tree is derivable from the non-terminal iff it
       is derivable. However, if we assume it is derivable, there is not a finite
       number of steps for the derivation, so we conclude false *)
    let rec _is_sent_element_non_term (non_term : non_terminal)
        (sent_tree : sent_tree) (productions : productions)
        (queried : NonTermSentTreeSet.t) =
      if NonTermSentTreeSet.mem (non_term, sent_tree) queried then false
        (* Encountered a loop in grammar: will not have finite derivation *)
      else
        let new_queried =
          NonTermSentTreeSet.add (non_term, sent_tree) queried
        in
        let sent_trees = productions non_term in
        SentTreeSet.exists
          (fun src_sent_tree ->
            _has_sent_derivation src_sent_tree sent_tree productions new_queried)
          sent_trees

    (* NOTE: this implicitly raises InvalidArgument if the tree/sent tree are not in the alphabet because of bad arity
       Should we check that somewhere? Probably as a precondition to calling the function, in the public is_element function*)
    and _has_sent_derivation (src_sent_tree : sent_tree)
        (dest_sent_tree : sent_tree) (productions : productions)
        (queried : NonTermSentTreeSet.t) =
      match (src_sent_tree, dest_sent_tree) with
      | `Symbol _, `NonTerminal _ ->
          (* Would require backwards derivation *)
          false
      | `NonTerminal src_non_term, _ ->
          (* Confirm that the non-terminal derives to the destination tree, which may be a symbol or non-terminal *)
          _is_sent_element_non_term src_non_term dest_sent_tree productions
            queried
      | `Symbol (src_symbol, src_children), `Symbol (dest_symbol, dest_children)
        ->
          (* Confirm symbols match and children are derivable *)
          src_symbol = dest_symbol
          (* TODO: use List.for_all2 instead *)
          && List.for_all
               (fun (src_child, dest_child) ->
                 _has_sent_derivation src_child dest_child productions queried)
               (List.combine src_children dest_children)

    let _production_valid (production : sent_tree list) =
      List.for_all sent_tree_in_alphabet production

    let rec _non_terms_in_sent_tree (sent_tree : sent_tree) =
      match sent_tree with
      | `NonTerminal non_term -> NonTermSet.singleton non_term
      | `Symbol (_, children) ->
          List.fold_left NonTermSet.union NonTermSet.empty
            (List.map _non_terms_in_sent_tree children)

    let rec _reachable_non_terms_rec (start : non_terminal)
        (productions : non_terminal -> sent_tree list) (visited : NonTermSet.t)
        =
      if NonTermSet.mem start visited then
        (* The non terminal has already been processed, so skip processing it again *)
        NonTermSet.empty
      else
        let sent_trees = productions start in
        let neighbors =
          List.fold_left NonTermSet.union NonTermSet.empty
            (List.map _non_terms_in_sent_tree sent_trees)
        in
        let new_visited = NonTermSet.add start visited in
        let accumulate_non_terms visited neighbor =
          NonTermSet.union visited
            (_reachable_non_terms_rec neighbor productions visited)
        in
        Seq.fold_left accumulate_non_terms new_visited
          (NonTermSet.to_seq neighbors)

    let _reachable_non_terms (start : non_terminal)
        (productions : non_terminal -> sent_tree list) =
      _reachable_non_terms_rec start productions NonTermSet.empty

    (* TODO: consider extracting the non-terminals by crawling the productions *)
    (* TODO: document that this function will raise an exception if productinos are invalid, and what the *)
    let create_grammar (start : non_terminal)
        (productions : non_terminal -> sent_tree list) =
      let all_non_terms = _reachable_non_terms start productions in
      let all_productions_valid =
        NonTermSet.for_all
          (fun non_term -> _production_valid (productions non_term))
          all_non_terms
      in
      if all_productions_valid then
        {
          start;
          productions =
            (fun non_term -> SentTreeSet.of_list (productions non_term));
        }
      else
        raise
          (Invalid_production
             "One of more of the provided productions contained invalid \
              sentential trees")

    let is_sent_element (sent_tree : sent_tree) (grammar : grammar) =
      _is_sent_element_non_term grammar.start sent_tree grammar.productions
        NonTermSentTreeSet.empty

    let is_element (tree : tree) (grammar : grammar) =
      if tree_in_alphabet tree then is_sent_element (tree :> sent_tree) grammar
      else
        raise
          (Invalid_tree
             "The provided tree did not belong to the grammar's alphabet")
  end

module type MAKE_FUNCTOR = functor (Alphabet : REG_TREE_ALPHABET) ->
  REG_TREE_GRAMMAR
    with type symbol = Alphabet.symbol
     and type non_terminal = Alphabet.non_terminal

module Make : MAKE_FUNCTOR = RegTreeGrammarImplementation
