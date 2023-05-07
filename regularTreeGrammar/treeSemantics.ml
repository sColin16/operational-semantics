module type TREE_SEMANTICS = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET
  module Tree : Tree.TREE with module RankedAlphabet := RankedAlphabet

  module PatternTree :
    PatternTree.REGULAR_PATTERN_TREE
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet

  module Semantics :
    Semantics.SEMANTICS
      with type term = Tree.t
       and type eval_rule =
        (PatternTree.t * PatternTree.t) list * (PatternTree.t * PatternTree.t)
end

module type TREE_SEMANTICS_INPUT = sig
  module TreeGrammar : TreeGrammar.TREE_GRAMMAR

  val start_non_terminal : TreeGrammar.non_terminal

  val productions :
    TreeGrammar.non_terminal -> TreeGrammar.SententialTree.t list
end

module TreeSemanticsImpl =
functor
  (PatternTreeMake : PatternTree.MAKE_FUNCTOR)
  (SemanticsMake : Semantics.MAKE_FUNCTOR)
  (Input : TREE_SEMANTICS_INPUT)
  ->
  struct
    type non_terminal = Input.TreeGrammar.non_terminal

    module RankedAlphabet = Input.TreeGrammar.RankedAlphabet
    module Tree = Input.TreeGrammar.Tree

    module PatternTree = PatternTreeMake (struct
      type non_terminal = Input.TreeGrammar.non_terminal

      module RankedAlphabet = RankedAlphabet
    end)

    module GrammarCollection = struct
      type grammar = Input.TreeGrammar.Grammar.t
      type non_terminal = Input.TreeGrammar.non_terminal

      let start_non_terminal = Input.start_non_terminal

      let mapping non_terminal =
        Input.TreeGrammar.Grammar.of_productions non_terminal Input.productions
    end

    module Semantics = SemanticsMake (struct
      module RankedAlphabet = Input.TreeGrammar.RankedAlphabet

      type non_terminal = Input.TreeGrammar.non_terminal

      module PatternTree = PatternTree
      module TreeGrammar = Input.TreeGrammar
      module GrammarCollection = GrammarCollection
    end)
  end

module type MAKE_FUNCTOR = functor (Input : TREE_SEMANTICS_INPUT) ->
  TREE_SEMANTICS
    with type non_terminal = Input.TreeGrammar.non_terminal
     and module RankedAlphabet = Input.TreeGrammar.RankedAlphabet
     and module Tree = Input.TreeGrammar.Tree

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : PatternTree.MAKE_FUNCTOR)
  (_ : Semantics.MAKE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = TreeSemanticsImpl
module Make : MAKE_FUNCTOR = CustomMake (PatternTree.Make) (Semantics.Make)
