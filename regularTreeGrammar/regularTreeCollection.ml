module type INPUT = sig
  type symbol
  type non_terminal

  type sent_node =
  | Symbol of symbol
  | NonTerminal of non_terminal

  module RankedAlpha : Common.RANKED_ALPHABET with type symbol = symbol
  module SentRankedAlpha : Common.RANKED_ALPHABET with type symbol = sent_node
end

module type INPUT2 = sig
  type symbol
  type non_terminal
  val arity : symbol -> int
end

module InputImpl = functor(Input : INPUT2) -> struct
  type symbol = Input.symbol
  type non_terminal = Input.non_terminal

  type sent_node =
  | Symbol of symbol
  | NonTerminal of non_terminal

  module RankedAlpha = struct
    type symbol = Input.symbol
    let arity = Input.arity
  end

  module SentRankedAlpha = struct
    type symbol = sent_node
    let arity node = match node with Symbol s -> Input.arity s | NonTerminal _ -> 0
  end
end

module type REGULAR_TREE_COLLECTION = sig
  module Input : INPUT

  type symbol = Input.symbol
  type non_terminal = Input.non_terminal
  type sent_node = Input.sent_node

  module Tree : GenericTree.TREE with module RankedAlpha = Input.RankedAlpha
  module SentTree : GenericTree.TREE with module RankedAlpha = Input.SentRankedAlpha

  val as_sent_tree : Tree.t -> SentTree.t
end

module RegularTreeCollectionImpl =
functor
  (Input : INPUT)
  (TreeMakeFunctor : GenericTree.MAKE_FUNCTOR)
  (SentTreeMakeFunctor : GenericTree.MAKE_FUNCTOR)
  ->
  struct
    type symbol = Input.symbol
    type non_terminal = Input.non_terminal

    type sent_node = Input.sent_node

    module RankedAlpha = Input.RankedAlpha
    module SentRankedAlpha = Input.SentRankedAlpha

    module Tree = TreeMakeFunctor (RankedAlpha)
    module SentTree = SentTreeMakeFunctor (Input.SentRankedAlpha)

    let rec as_sent_tree (tree : Tree.t) =
      let symbol, children = Tree.destructure tree in
      SentTree.node (Symbol symbol) (List.map as_sent_tree children)
  end

module type MAKE_FUNCTOR = functor
  (Input : INPUT)
  (_ : GenericTree.MAKE_FUNCTOR)
  (_ : GenericTree.MAKE_FUNCTOR)
  ->
  REGULAR_TREE_COLLECTION
  with module Input := Input

module Make : MAKE_FUNCTOR = RegularTreeCollectionImpl

module MyInput2 = struct
  type symbol = A | B
  type non_terminal = C | D
  let arity symbol = match symbol with A | B -> 1
end

(* TODO: create a single functor that can do both of these things from the input above, and we'll be golden *)
(* Honestly I'm not sure how to do that. Come on OCaml be nice :( )*)
module MyInput : INPUT = InputImpl(MyInput2)
module Collection = Make(MyInput)(GenericTree.Make)(GenericTree.Make)
