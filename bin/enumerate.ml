(* tunable params *)

(* IO *)
let jsonl_filepath = ref ""
let motifs_filepath = ref ""
let yell = ref false

(* search parameterization *)
let strategy = ref ""
let sample_goal = ref 100
let beam_width = ref 100

(* cli *)
let spec_list = [
    ("--input", Arg.Set_string jsonl_filepath, "JSONL file representing positive neighborhoods");
    ("--output", Arg.Set_string motifs_filepath, "Output file to save synthesized motifs to");
    ("--strategy", Arg.Set_string strategy, "Sets synthesis strategy (see documentation)");
    ("--yell", Arg.Set yell, "Enables yelling (very loud output)");
    ("--sample-goal", Arg.Set_int sample_goal, "How many motifs should we sample (default: 100)");
    ("--beam-width", Arg.Set_int beam_width, "Width of search when using strategy: beam (default: 100)");
]

let usage_msg = "Motif Enumeration"
let _ = Arg.parse spec_list print_endline usage_msg

(* loading the motifs *)
let _ = print_endline "Loading input motifs..."
let motifs = Utility.JSON.of_jsonl Motifs.Motif.of_json !jsonl_filepath
let _ = print_endline ("Done. Found " ^ (motifs |> CCList.length |> string_of_int) ^ " motifs.")

(* convert to cone for synthesis *)
let cone = Synthesis.Cone.from_motifs motifs
let submotifs = match !strategy with
    | s when s = "enumerate" -> 
        Synthesis.Cone.enumerate
            ~verbose:(!yell)
        cone
    | s when s = "sample" -> 
        Synthesis.Cone.sample
            ~verbose:(!yell)
            ~count:(!sample_goal)
        cone
    | s when s = "beam" ->
        Synthesis.Cone.beam_search
            ~verbose:(!yell)
            ~width:(!beam_width)
        cone
    | _ -> print_endline "No synthesis strategy provided. Terminating synthesis..."; []
let _ = print_endline ("Done. Synthesized " ^ (submotifs |> CCList.length |> string_of_int) ^ " motifs.")
let _ = Utility.JSON.to_jsonl Motifs.Motif.to_json submotifs !motifs_filepath