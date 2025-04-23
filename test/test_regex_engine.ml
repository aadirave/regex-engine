open Regex_engine.Dfa
open Regex_engine.Nfa
open Stdio

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

let () = 
        let pattern = "a(b|d)" in
        let text = "ad" in

        printf "Pattern %s \n" pattern;
        printf "Text %s \n" text;
        printf "Result ";

        try
        let ast' = Regex_engine.Parser.main Regex_engine.Lexer.token (Lexing.from_string pattern) in
        let nfa' = nfa_from_ast ast' in
        let dfa' = nfa_to_dfa nfa' in
        let is_match = match_dfa dfa' text in

        if is_match then printf "Matched\n" else printf "Unmatched\n"

        with
                | Failure msg -> eprintf "\n\nRan into an error: %s\n" msg;
                        exit 1
                | ex -> eprintf "\n\nAn unexpected error occured: %s\n" (Printexc.to_string ex);
                        exit 1
