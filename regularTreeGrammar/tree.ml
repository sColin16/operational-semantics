(** A functional tree data structure where each node has a fixed, ordered number
    of children *)
module type TREE = sig
  type t

  module RankedAlphabet : Common.RANKED_ALPHABET
  (** The nodes and the number of children that those nodes must have *)

  exception Invalid_arity of string

  val node : RankedAlphabet.symbol -> t list -> t
  (** Constructs a tree from a node and a list of its child tree.
      Raises Invalid_arity if the arity invariant for the node is not met *)

  val destructure : t -> RankedAlphabet.symbol * t list
  (** Decomposes a tree into the root node its children trees *)
end

module TreeImpl =
functor
  (Alphabet : Common.RANKED_ALPHABET)
  ->
  struct
    module RankedAlphabet = Alphabet

    type t = Node of RankedAlphabet.symbol * t list

    exception Invalid_arity of string

    let node symbol children =
      if List.length children = RankedAlphabet.arity symbol then
        Node (symbol, children)
      else raise (Invalid_arity "Wrong number of children provided for symbol")

    let destructure tree =
      let (Node (symbol, children)) = tree in
      (symbol, children)
  end

module type MAKE_FUNCTOR = functor (Alphabet : Common.RANKED_ALPHABET) ->
  TREE with module RankedAlphabet = Alphabet

module Make : MAKE_FUNCTOR = TreeImpl
