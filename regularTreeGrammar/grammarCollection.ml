(* TODO: consider adding the primary non-terminal or grammar into here
   I feel like the function to create this will be from productions, and we will get the start non-terminal that way *)
module type GRAMMAR_COLLECTION = sig
  type grammar
  type non_terminal

  val mapping : non_terminal -> grammar
end
