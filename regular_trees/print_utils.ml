open Types

let non_term_label non_term =
    let NonTerminal(label) = non_term in label

let symbol_label symbol =
    let Symbol(label) = symbol in label

let print_non_terms set =
    let () = NonTerminalSet.iter (fun non_term -> Printf.printf "%s " (non_term_label non_term)) set in
    Printf.printf "\n"

let print_symbol s arity =
    Printf.printf "(%s, %i)\n" (symbol_label s) arity

let print_ranked_alpha alphabet =
    SymbolMap.iter print_symbol alphabet
