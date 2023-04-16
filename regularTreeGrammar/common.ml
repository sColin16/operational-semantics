(** Represents the mathematical concept of a ranked alphabet: a pair of symbols,
    and a function that provides the arity for each symbol. We use this as a
    module to get a sort of type that include both of these fields as a sort of
    atomic "type" *)
module type RANKED_ALPHABET = sig
  type symbol

  val arity : symbol -> int
end

module type REGULAR_TREE_ALPHABET = sig
  module RankedAlphabet : RANKED_ALPHABET

  type non_terminal
end
