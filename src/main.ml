open Regex_engine.Dfa

let explode_string (s : string) : char list =
        List.init (String.length s) (String.get s)

let match_dfa (dfa' : dfa) (input_str : string) : bool =
        let input_chars = explode_string input_str in

        let current_state = ref dfa'.start_state in
        let success = ref true in

        List.iter (fun c -> 
                if !success then 
                match DfaTransitions.find_opt (!current_state, c) dfa'.transitions with
                        | Some next_state -> current_state := next_state
                        | None -> success := false
        ) input_chars;

        !success && StateSet.mem !current_state dfa'.accept_states

