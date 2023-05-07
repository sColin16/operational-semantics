module type TREE_GRAMMAR = sig
  type non_terminal

  module RankedAlphabet : Common.RANKED_ALPHABET
  module Tree : Tree.TREE with module RankedAlphabet := RankedAlphabet

  module SententialTree :
    SententialTree.SENT_TREE
      with type non_terminal := non_terminal
       and module RankedAlphabet := RankedAlphabet

  module Grammar :
    Grammar.GRAMMAR
      with type non_terminal = non_terminal
       and type sentence = Tree.t
       and type sentential_form = SententialTree.t

  (* TODO: include function to convert back and forth between trees and sent trees? *)
end

module TreeGrammarMakeImpl =
functor
  (TreeMake : Tree.MAKE_FUNCTOR)
  (SentTreeMake : SententialTree.MAKE_FUNCTOR)
  (GrammarMake : Grammar.MAKE_FUNCTOR)
  (Alphabet : Common.REGULAR_TREE_ALPHABET)
  ->
  struct
    type non_terminal = Alphabet.non_terminal

    module RankedAlphabet = Alphabet.RankedAlphabet
    module Tree = TreeMake (Alphabet.RankedAlphabet)
    module SententialTree = SentTreeMake (Alphabet)

    module Grammar = GrammarMake (struct
      type non_terminal = Alphabet.non_terminal

      module RankedAlphabet = Alphabet.RankedAlphabet
      module Tree = Tree
      module SentTree = SententialTree
    end)
  end

module type MAKE_FUNCTOR = functor (Alphabet : Common.REGULAR_TREE_ALPHABET) ->
  TREE_GRAMMAR
    with type non_terminal = Alphabet.non_terminal
     and module RankedAlphabet = Alphabet.RankedAlphabet

module type CUSTOM_MAKE_FUNCTOR = functor
  (_ : Tree.MAKE_FUNCTOR)
  (_ : SententialTree.MAKE_FUNCTOR)
  (_ : Grammar.MAKE_FUNCTOR)
  -> MAKE_FUNCTOR

module CustomMake : CUSTOM_MAKE_FUNCTOR = TreeGrammarMakeImpl
module Make : MAKE_FUNCTOR = CustomMake (Tree.Make) (SententialTree.Make) (Grammar.Make)
