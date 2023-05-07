module type GRAMMAR_COLLECTION = sig
  type grammar
  type non_terminal

  val start_non_terminal : non_terminal
  val mapping : non_terminal -> grammar
end
