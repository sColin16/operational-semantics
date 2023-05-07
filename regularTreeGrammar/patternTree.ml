module type REGULAR_PATTERN_TREE = sig
  type t

  type non_terminal
  (** non-terminal nodes have arity 0 by default *)

  module RankedAlphabet : Common.RANKED_ALPHABET
  (** The nodes and the number of children that those nodes must have *)

  type node =
    | Symbol of RankedAlphabet.symbol * t list
    | NonTerminal of non_terminal * int

  val symbol : RankedAlphabet.symbol -> t list -> t
  (** Constructs a tree from a symbol and a list of its children trees
    Raises Invalid_arity if the arity invariant for the node is not met *)

  val non_terminal : non_terminal -> int -> t
  (** Constructs a labeled non-terminal node in a pattern tree, which is
      effectively a metavarivable node *)

  val destructure : t -> node
  (** Decomposes a tree into the root node its children trees *)
end

module PatternTreeImpl =
functor
  (SentTreeMake : SententialTree.MAKE_FUNCTOR)
  (Alphabet : Common.REGULAR_TREE_ALPHABET)
  ->
  struct
    type non_terminal = Alphabet.non_terminal

    module RankedAlphabet = Alphabet.RankedAlphabet

    type labeled_non_term = non_terminal * int

    module SentTree = SentTreeMake (struct
      type non_terminal = labeled_non_term

      module RankedAlphabet = Alphabet.RankedAlphabet
    end)

    type t = SentTree.t

    type node =
      | Symbol of RankedAlphabet.symbol * t list
      | NonTerminal of non_terminal * int

    let symbol symbol children =
      try SentTree.symbol symbol children
      with Invalid_argument s -> raise (Invalid_argument s)

    let non_terminal non_term label = SentTree.non_terminal (non_term, label)

    let destructure tree =
      match SentTree.destructure tree with
      | Symbol (symbol, children) -> Symbol (symbol, children)
      | NonTerminal (non_term, label) -> NonTerminal (non_term, label)
  end

module type MAKE_FUNCTOR = functor (Alphabet : Common.REGULAR_TREE_ALPHABET) ->
  REGULAR_PATTERN_TREE
    with module RankedAlphabet = Alphabet.RankedAlphabet
     and type non_terminal = Alphabet.non_terminal

module type CUSTOM_MAKE_FUNCTOR = functor (_ : SententialTree.MAKE_FUNCTOR) ->
  MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = PatternTreeImpl
module Make : MAKE_FUNCTOR = PatternTreeImpl (SententialTree.Make)
