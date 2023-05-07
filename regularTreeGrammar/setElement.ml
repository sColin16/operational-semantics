(* I'm thinking that we have separate implementations for the grammar element for tree and sentential trees
    Because I feel like the signature will have duplicate types and functions in it to handle both

    I'm not 100% sure on this, maybe it would be better to just combine them since it is grammar-specific
    and not some general item is a member of a language or set construct

    Let's try to use it in other things and see where we get

    Generally, we really only need the sentential form one, because we can convert between tree types
    and concrete trees are a subset of sentential trees
*)

(* How useful is this module really? I don't really know if I want this in any prublic-facing
   types. I think it would only be an internal module for implementations *)

(** A module whose type is guaranteed to belong to some set, usually an infinite
    one. This allows you to express a type for a value that belongs to a set that
    is not directable expressable with a standard type. It can't be type-checked
    like usual types, but it better expresses the intent of a value in some cases *)
module type SET_ELEMENT = sig
  type t
  type elt
  type set

  val create_opt : elt -> set -> t option
  (** [create_opt elt set] returns the set element as an option
      if it is valid, otherwise [None] *)

  val unwrap : t -> elt
  (** [unwrap set_element] returns the underlying element that is a member of the set *)
end

module SentenceElementImpl =
functor
  (Grammar : Grammar.GRAMMAR)
  ->
  struct
    type elt = Grammar.sentence
    type set = Grammar.t
    type t = elt

    let create_opt sentence grammar =
      if Grammar.is_element sentence grammar then Some sentence else None

    let unwrap grammar_element = grammar_element
  end

module SententialFormElementImpl =
functor
  (Grammar : Grammar.GRAMMAR)
  ->
  struct
    type elt = Grammar.sentential_form
    type set = Grammar.t
    type t = elt

    let create_opt sentential_form grammar =
      if Grammar.has_derivation sentential_form grammar then
        Some sentential_form
      else None

    let unwrap grammar_element = grammar_element
  end

module type MAKE_SENTENCE_FUNCTOR = functor (Grammar : Grammar.GRAMMAR) ->
  SET_ELEMENT with type elt = Grammar.sentence and type set = Grammar.t

module type MAKE_SENTENTIAL_FORM_FUNCTOR = functor
  (Grammar : Grammar.GRAMMAR)
  ->
  SET_ELEMENT with type elt = Grammar.sentential_form and type set = Grammar.t

module MakeSentence : MAKE_SENTENCE_FUNCTOR = SentenceElementImpl

module MakeSententialForm : MAKE_SENTENTIAL_FORM_FUNCTOR =
  SententialFormElementImpl

module type PATTERN_TREE_INPUT = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET

  module TreeGrammar :
    TreeGrammar.TREE_GRAMMAR
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet

  module PatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet
end

module PatternTreeImpl =
functor
  (SentTreeElementMake : MAKE_SENTENTIAL_FORM_FUNCTOR)
  (Input : PATTERN_TREE_INPUT)
  ->
  struct
    type elt = Input.PatternTree.t
    type set = Input.TreeGrammar.Grammar.t
    type t = elt

    module SentTreeElement = SentTreeElementMake (struct
      include Input.TreeGrammar.Grammar

      type non_terminal = Input.non_terminal
      type sentence = Input.TreeGrammar.Tree.t
      type sentential_form = Input.TreeGrammar.SententialTree.t
    end)

    let rec as_sent_tree pattern =
      match Input.PatternTree.destructure pattern with
      | Symbol (symbol, children) ->
          Input.TreeGrammar.SententialTree.symbol symbol (List.map as_sent_tree children)
      | NonTerminal (non_term, _) -> Input.TreeGrammar.SententialTree.non_terminal non_term

    let create_opt pattern grammar =
      match SentTreeElement.create_opt (as_sent_tree pattern) grammar with
      | Some _ -> Some pattern
      | None -> None

    let unwrap pattern_element = pattern_element
  end

module type MAKE_PATTERN_TREE_FUNCTOR = functor (Input : PATTERN_TREE_INPUT) ->
  SET_ELEMENT with type elt = Input.PatternTree.t and type set = Input.TreeGrammar.Grammar.t

module type CUSTOM_MAKE_PATTERN_TREE_FUNCTOR = functor
  (_ : MAKE_SENTENTIAL_FORM_FUNCTOR)
  -> MAKE_PATTERN_TREE_FUNCTOR

module CustomMakePatternTree : CUSTOM_MAKE_PATTERN_TREE_FUNCTOR =
  PatternTreeImpl

module MakePatternTree : MAKE_PATTERN_TREE_FUNCTOR =
  PatternTreeImpl (MakeSententialForm)
