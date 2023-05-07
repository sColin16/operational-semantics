module type EVAL_RULE_VERIFIER = sig
  type premises
  type conclusion

  (* TODO: is this unnecessarily abstract? *)
  val order_premises : premises -> conclusion -> premises option
  (** [order_premises premises conclusion] verifies that the premises and
      conclusion are valid and returns an ordered list in which the premises can
      be evaluated if so *)
end

(** An internal module to validate the integrity of an evaluation rule *)
module EvalRuleImpl =
functor
  (Input : Pattern.REGULAR_PATTERN)
  ->
  struct
    type eval_relation = Input.Pattern.t * Input.Pattern.t
    type metavariable = Input.non_terminal * int
    type premises = eval_relation list
    type conclusion = eval_relation

    (* Represents a metavariable, a non-terminal, integer pair *)
    type metavariable_node = {
      non_term : Input.non_terminal;
      label : int;
      mutable dependent_rules : rule_node list;
      mutable visited : bool;
    }

    (* Represents either a premise, or a left/right conclusion pattern *)
    and rule_node = {
      premise : eval_relation option;
      mutable num_deps_left : int;
      revealed_metavariables : metavariable_node list;
          (* I kinda feel like this should be a set *)
          (* But making it a set of a type with mutable fields seems strange and I feel like the graph should be all connected, not depend on an external map to resolve *)
          (* We just need to make sure that there aren't repeats in the list. Not that it would actually break anything though. At least as far as I know. But still that's extra complexity to track *)
    }

    module MetavariableSet = Set.Make (struct
      type t = metavariable

      let compare = compare
    end)

    module MetavariableMap = Map.Make (struct
      type t = metavariable

      let compare = compare
    end)

    type meta_map = metavariable_node MetavariableMap.t

    type graph = {
      conclusion_left : rule_node;
      conclusion_right : rule_node;
      premises : rule_node list;
    }

    type node = RuleNode of rule_node | MetaNode of metavariable_node

    (** Extracts all the metavariables (labeled non-terminals) from a pattern *)
    let rec extract_metavariables (pattern : Input.Pattern.t) =
      extract_metavariables_rec (Input.Pattern.unwrap pattern)

    and extract_metavariables_rec (pattern : Input.RegularPatternTree.t) =
      match Input.RegularPatternTree.destructure pattern with
      | Symbol (_, children) ->
          List.fold_left MetavariableSet.union MetavariableSet.empty
            (List.map extract_metavariables_rec children)
      | NonTerminal (non_term, label) ->
          MetavariableSet.singleton (non_term, label)

    (** Gets the corresponding node for the given metavariable, creating it as
      necessary. Mutates the metavariable map with the new node if applicable *)
    let get_metavariable_node (meta_map : meta_map ref)
        (metavariable : metavariable) : metavariable_node =
      let non_term, label = metavariable in
      match MetavariableMap.find_opt metavariable !meta_map with
      | Some meta_node -> meta_node
      | None ->
          let new_node =
            { non_term; label; dependent_rules = []; visited = false }
          in
          meta_map := MetavariableMap.add metavariable new_node !meta_map;
          new_node

    (** Gets the metavariable nodes for a set of metavariables. Although it returns a list which could have duplicates, the input type prevents duplicates *)
    let get_metavariable_nodes (meta_map : meta_map ref)
        (metavariables : MetavariableSet.t) : metavariable_node list =
      let metavariable_list =
        List.of_seq (MetavariableSet.to_seq metavariables)
      in
      List.map (get_metavariable_node meta_map) metavariable_list

    (** Marks a rule_node as a being dependent on a metavariable. That is, the
     metavariable must be resolved before the rule can be tested *)
    let mark_rule_dependency (meta_map : meta_map ref) (rule_node : rule_node)
        (metavariable : metavariable) =
      let meta_node = get_metavariable_node meta_map metavariable in
      meta_node.dependent_rules <- rule_node :: meta_node.dependent_rules;
      ()

    (** Marks a rule_node as being dependent on a set of metavariables. That is,
          all metavariables in the set must be resolved before the rule can be tested *)
    let mark_rule_dependencies (meta_map : meta_map ref) (rule_node : rule_node)
        (metavariables : MetavariableSet.t) =
      let metavariable_list =
        List.of_seq (MetavariableSet.to_seq metavariables)
      in
      List.iter (mark_rule_dependency meta_map rule_node) metavariable_list

    (** Creates the rule_node for a premise *)
    let create_premise_node (meta_map : meta_map ref) (premise : eval_relation)
        =
      let premise_left, premise_right = premise in
      let left_metavariables = extract_metavariables premise_left in
      let right_metavariables = extract_metavariables premise_right in
      let premise_node =
        {
          premise = Some premise;
          num_deps_left = MetavariableSet.cardinal left_metavariables;
          revealed_metavariables =
            get_metavariable_nodes meta_map right_metavariables;
        }
      in
      mark_rule_dependencies meta_map premise_node left_metavariables;
      premise_node

    let create_premise_nodes (meta_map : meta_map ref)
        (premises : eval_relation list) =
      List.map (create_premise_node meta_map) premises

    (** Constructs the dependency graph for the premises of a single evaluation rule *)
    let make_graph (premises : eval_relation list) (conclusion : eval_relation)
        : graph =
      let conc_left, conc_right = conclusion in
      let meta_map = ref MetavariableMap.empty in
      let conc_left_meta_nodes =
        get_metavariable_nodes meta_map (extract_metavariables conc_left)
      in
      let conc_left_node =
        {
          premise = None;
          num_deps_left = 0;
          revealed_metavariables = conc_left_meta_nodes;
        }
      in
      let conc_right_metavariables = extract_metavariables conc_right in
      let conc_right_node =
        {
          premise = None;
          num_deps_left = MetavariableSet.cardinal conc_right_metavariables;
          revealed_metavariables = [];
        }
      in
      mark_rule_dependencies meta_map conc_right_node conc_right_metavariables;
      let premise_nodes = create_premise_nodes meta_map premises in
      {
        conclusion_left = conc_left_node;
        conclusion_right = conc_right_node;
        premises = premise_nodes;
      }

    (** Performs the search on the dependent graph to determine the order that the premises can be evaluated in, if any *)
    let eval_graph (graph : graph) : eval_relation list option =
      (* Store a reversed list of when we evaluate premises, since it's a linked list *)
      let rev_ordered_premises = ref [] in
      (* Start the stack with any node with 0 dependencies, which should include conclusion_left *)
      let stack =
        ref
          (List.filter_map
             (fun node ->
               if node.num_deps_left = 0 then Some (RuleNode node) else None)
             (graph.conclusion_left :: graph.conclusion_right :: graph.premises))
      in
      (* TODO: is there some declarative data structure where we can do this iteration over the stack w/o a while loop? *)
      let () =
        (* Perform DFS, with catch of honoring the dependency number for rule nodes *)
        while List.length !stack > 0 do
          match !stack with
          | [] ->
              () (* This case is not possible because of while loop condition *)
          | head :: tail -> (
              (* Pop from the stack by reassigning the tail to the reference *)
              stack := tail;
              match head with
              | RuleNode node ->
                  (* For rule nodes, start by extracting the premise, if this is a premise node. Otherwise it's a conclusion so it shouldn't be in the in the premise list *)
                  (match node.premise with
                  | None -> ()
                  | Some premise ->
                      rev_ordered_premises := premise :: !rev_ordered_premises);
                  (* Add each non-visited revealed metavariable to the stack, and mark it as visited *)
                  List.iter
                    (fun meta_node ->
                      if not meta_node.visited then
                        stack := MetaNode meta_node :: !stack;
                      meta_node.visited <- true)
                    node.revealed_metavariables
              | MetaNode node ->
                  (* Only consider dependent rule nodes with at least one dependency left, to avoid reprocessing already visited nodes *)
                  let unvisited_nodes =
                    List.filter
                      (fun node -> node.num_deps_left > 0)
                      node.dependent_rules
                  in
                  (* Decrement the number of dependencies for each rule node *)
                  List.iter
                    (fun rule_node ->
                      rule_node.num_deps_left <- rule_node.num_deps_left - 1)
                    unvisited_nodes;
                  (* Each rule node that has 0 depdencies as a result should be added to the stack *)
                  List.iter
                    (fun rule_node ->
                      if rule_node.num_deps_left = 0 then
                        stack := RuleNode rule_node :: !stack)
                    unvisited_nodes)
        done
      in
      if
        (* If the RHS of the conclusion and all premises can have metavariables revealed, return the order to evaluate premises in *)
        (* Otherwise, we won't be able to obtain an assignment for some metavariable, so return None *)
        graph.conclusion_right.num_deps_left = 0
        && List.for_all
             (fun premise_node -> premise_node.num_deps_left = 0)
             graph.premises
      then Some (List.rev !rev_ordered_premises)
      else None

    (** [order_premises premises conclusion] verifies that the premises and
      conclusion are valid and returns an ordered list in which the premises can
      be evaluated if so *)
    let order_premises premises conclusion =
      let graph = make_graph premises conclusion in
      eval_graph graph
  end

module type MAKE_FUNCTOR = functor (Input : Pattern.REGULAR_PATTERN) ->
  EVAL_RULE_VERIFIER
    with type premises = (Input.Pattern.t * Input.Pattern.t) list
     and type conclusion = Input.Pattern.t * Input.Pattern.t

module Make : MAKE_FUNCTOR = EvalRuleImpl
