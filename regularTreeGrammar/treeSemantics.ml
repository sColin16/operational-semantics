module type TREE_SEMANTICS = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET
  module TreeGrammar : TreeGrammar.TREE_GRAMMAR
end
