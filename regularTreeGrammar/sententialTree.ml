(** A functional data structure to represent sentential forms of trees, which
    may consist of nodes and non-terminals *)
module type SENT_TREE = sig
  type t

  type non_terminal
  (** non-terminal nodes have arity 0 by default *)

  module RankedAlphabet : Common.RANKED_ALPHABET
  (** The nodes and the number of children that those nodes must have *)

  type node =
    | Symbol of RankedAlphabet.symbol * t list
    | NonTerminal of non_terminal

  val symbol : RankedAlphabet.symbol -> t list -> t
  (** Constructs a tree from a symbol and a list of its children trees
      Raises Invalid_arity if the arity invariant for the node is not met *)

  val non_terminal : non_terminal -> t
  (** Constructs a non-terminal node in a sentential tree, which may not
      have any children nodes or trees *)

  val destructure : t -> node
  (** Decomposes a tree into the root node its children trees *)
end

module SentTreeImpl =
functor
  (TreeMake : Tree.MAKE_FUNCTOR)
  (Alphabet : Common.REGULAR_TREE_ALPHABET)
  ->
  struct
    type non_terminal = Alphabet.non_terminal

    module RankedAlphabet = Alphabet.RankedAlphabet

    type internal_symbol =
      | IntSymbol of RankedAlphabet.symbol
      | IntNonTerminal of non_terminal

    module Tree = TreeMake (struct
      type symbol = internal_symbol

      let arity node =
        match node with
        | IntSymbol s -> RankedAlphabet.arity s
        | IntNonTerminal _ -> 0
    end)

    type t = Tree.t

    type node =
      | Symbol of RankedAlphabet.symbol * t list
      | NonTerminal of non_terminal

    let symbol symbol children =
      try Tree.node (IntSymbol symbol) children
      with Invalid_argument s -> raise (Invalid_argument s)

    let non_terminal non_term = Tree.node (IntNonTerminal non_term) []

    let destructure tree =
      match Tree.destructure tree with
      | IntSymbol s, children -> Symbol (s, children)
      | IntNonTerminal t, _ -> NonTerminal t
  end

module type MAKE_FUNCTOR = functor (Alphabet : Common.REGULAR_TREE_ALPHABET) ->
  SENT_TREE
    with module RankedAlphabet = Alphabet.RankedAlphabet
     and type non_terminal = Alphabet.non_terminal

module type CUSTOM_MAKE_FUNCTOR = functor (_ : Tree.MAKE_FUNCTOR) ->
  MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = SentTreeImpl
module Make : MAKE_FUNCTOR = SentTreeImpl (Tree.Make)
