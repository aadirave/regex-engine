open Ast
open Nfa

module StateSet = Set.Make(Int)

module DfaStateMap = Map.Make(Int)
module CharMap = Map.Make(Char)

module DfaTransitions = Map.Make(struct
  type t = int * char
  let compare = compare
end)

type dfa = {
  num_states: int;
  transitions: int DfaTransitions.t;
  start_state: int;
  accept_states: StateSet.t;
  dfa_to_nfa_states: StateSet.t DfaStateMap.t;
}

let get_alphabet (nfa: nfa) : char list = 
  Transitions.fold (fun (_, char_opt) _ acc -> 
    match char_opt with
      | Some c -> if List.mem c acc then acc else c :: acc
      | None -> acc
  ) nfa.transitions []

(* Initial call should be something like
   epsilon_closure nfa (StateSet.singleton nfa.start_state)
*)
let rec epsilon_closure (nfa' : nfa) (states : StateSet.t) : StateSet.t =
  let direct_eps_reachable =
    StateSet.fold (fun state acc ->
      match Transitions.find_opt (state, None) nfa'.transitions with
      | Some next_states -> StateSet.union next_states acc
      | None -> acc) states StateSet.empty
  in
  if StateSet.subset direct_eps_reachable states then
    states
  else
    epsilon_closure nfa' (StateSet.union states direct_eps_reachable)

(* Find all reachable states from a given state without following any epsilon transitions *)
let move (nfa' : nfa) (states : StateSet.t) (c : char) : StateSet.t = 
  StateSet.fold (fun state acc ->
    match Transitions.find_opt (state, Some c) nfa'.transitions with
      | Some next_states -> StateSet.union next_states acc
      | None -> acc ) states StateSet.empty

(* Build the dfa states and transitions *)
let rec nfa_to_dfa (nfa' : nfa) : dfa =
  let alphabet = get_alphabet nfa' in

  let module NfaStateSetMap = Map.Make(StateSet) in

  let discovered_states = ref NfaStateSetMap.empty in
  let dfa_id_to_nfa_set = ref DfaStateMap.empty in
  let dfa_state_count = ref 0 in
  let worklist = Queue.create () in
  let dfa_transitions = ref DfaTransitions.empty in 
  let dfa_accept_states = ref StateSet.empty in

  let get_or_create_dfa_id (nfa_set : StateSet.t) : int = 
    match NfaStateSetMap.find_opt nfa_set !discovered_states with
    | Some id -> id
    | None ->
      let new_id = !dfa_state_count in
          incr dfa_state_count;
          discovered_states := NfaStateSetMap.add nfa_set new_id !discovered_states;
          dfa_id_to_nfa_set := DfaStateMap.add new_id nfa_set !dfa_id_to_nfa_set;
          Queue.add nfa_set worklist;
          new_id
  in

  let init_nfa_set = epsilon_closure nfa' (StateSet.singleton nfa'.start_state) in
  let dfa_start_state_id = get_or_create_dfa_id init_nfa_set in

  while not (Queue.is_empty worklist) do
    let current_nfa_set = Queue.take worklist in
    let current_dfa_id = NfaStateSetMap.find current_nfa_set !discovered_states in (* Should always exist *)

    if not (StateSet.is_empty (StateSet.inter (StateSet.singleton nfa'.accepting_state) current_nfa_set)) then
      dfa_accept_states := StateSet.add current_dfa_id !dfa_accept_states;

    List.iter (fun c ->
      let next_nfa_states_direct = move nfa' current_nfa_set c in
      if not (StateSet.is_empty next_nfa_states_direct) then
        let next_nfa_set_closure = epsilon_closure nfa' next_nfa_states_direct in
        let next_dfa_id = get_or_create_dfa_id next_nfa_set_closure in
        dfa_transitions := DfaTransitions.add (current_dfa_id, c) next_dfa_id !dfa_transitions
    ) alphabet
  done;

  {
    num_states = !dfa_state_count;
    transitions = !dfa_transitions;
    start_state = dfa_start_state_id;
    accept_states = !dfa_accept_states;
    dfa_to_nfa_states = !dfa_id_to_nfa_set;
  }

