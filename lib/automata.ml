open Ast

module StateSet = Set.Make(Int)
module Transitions = Map.Make(struct 
  type t = int * char option
  let compare = compare
end)

type nfa = {
  states: StateSet.t;
  transitions: StateSet.t Transitions.t;
  start_state: int;
  accepting_state: int;
}

let state_counter = ref(-1)
let fresh_state () = 
  incr state_counter;
  !state_counter

let rec nfa_from_ast (ast: regex_t) : nfa =
  match ast with
  | Empty ->
  let start = fresh_state () in
  let accept = fresh_state () in
    {
      states = StateSet.of_list [start; accept];
      transitions = Transitions.singleton (start, None) (StateSet.singleton accept);
      start_state = start;
      accepting_state = accept;
    }
  | Char c ->
  let start = fresh_state () in
  let accept = fresh_state () in
    {
      states = StateSet.of_list [start; accept];
      transitions = Transitions.singleton (start, Some c) (StateSet.singleton accept);
      start_state = start;
      accepting_state = accept;
    }
  | Concat (left, right) ->
  let left_nfa = nfa_from_ast left in
  let right_nfa = nfa_from_ast right in

  let states = StateSet.union left_nfa.states right_nfa.states in
  let trans = Transitions.union (fun _ _ _ -> None) left_nfa.transitions right_nfa.transitions in
  let trans_plus_eps = Transitions.add (left_nfa.accepting_state, None) (StateSet.singleton right_nfa.start_state) trans in
    {
      states = states;
      transitions = trans_plus_eps;
      start_state = left_nfa.start_state;
      accepting_state = right_nfa.accepting_state;
    }
  | Choice (left, right) ->
  let left_nfa = nfa_from_ast left in
  let right_nfa = nfa_from_ast right in

  let start = fresh_state () in
  let accept = fresh_state () in

  let states = StateSet.union left_nfa.states right_nfa.states |> StateSet.add start |> StateSet.add accept in
  let trans = Transitions.union (fun _ _ _ -> None) left_nfa.transitions right_nfa.transitions in
  let trans_plus_eps = trans
  |> Transitions.add (start, None) (StateSet.of_list [left_nfa.start_state; right_nfa.start_state])
  |> Transitions.add (left_nfa.accepting_state, None) (StateSet.singleton accept)
  |> Transitions.add (right_nfa.accepting_state, None) (StateSet.singleton accept)
    in 
    {
      states = states;
      transitions = trans_plus_eps;
      start_state = start;
      accepting_state = accept;
    }
  | Star ast' ->
  let nfa' = nfa_from_ast ast' in
  let start = fresh_state () in
  let accept = fresh_state () in

  let states = StateSet.add start (StateSet.add accept nfa'.states) in
  let trans_plus_eps = nfa'.transitions
  |> Transitions.add (start, None) (StateSet.of_list [nfa'.start_state; accept])
  |> Transitions.add (nfa'.accepting_state, None) (StateSet.of_list [nfa'.start_state; accept])
    in
    {
      states = states;
      transitions = trans_plus_eps;
      start_state = start;
      accepting_state = accept;
    }


