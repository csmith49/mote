let output_file = ref ""
let data_file = ref ""
let motif_file = ref ""

let spec_list = [
    ("--data", Arg.Set_string data_file, "Filenames containing data to be evaluated");
    ("--output", Arg.Set_string output_file, "Output file");
    ("--motifs", Arg.Set_string motif_file, "Synthesized motifs");
]

let usage_msg = "Motif Evaluation"
let _ = Arg.parse spec_list print_endline usage_msg

let _ = print_endline "Starting evaluation..."

(* loading the motifs *)
let _ = print_endline "Loading data files..."
let files = Utility.JSON.of_jsonl (Utility.JSON.get "filename" Utility.JSON.string) !data_file
let num_files = CCList.length files
let _ = print_endline ("Done. Found " ^ (num_files |> string_of_int) ^ " data files.")

(* loading the motifs *)
let _ = print_endline "Loading input motifs..."
let motifs = Utility.JSON.of_jsonl Motifs.Motif.of_json !motif_file
let _ = print_endline ("Done. Found " ^ (motifs |> CCList.length |> string_of_int) ^ " motifs.")


(* now build the sparse image *)
let _ = print_endline "Building image..."
let sparse_image = ref (Domain.SparseImage.of_motifs motifs)
let _ = CCList.iteri (fun i -> fun filename ->
    let _ = Printf.printf "Evaluating %s (%d/%d)\n%!" filename i num_files in
    let db = Domain.SQL.of_string filename in
    let _ = Printf.printf "We did it" in
    let images = CCList.map (Domain.SQL.evaluate db) motifs in
    let _ = Domain.SQL.close db in
    sparse_image := Domain.SparseImage.add_results filename images !sparse_image
) files
let _ = Yojson.Basic.to_file !output_file (Domain.SparseImage.to_json !sparse_image)
let _ = Printf.printf "...done. Output written to %s\n" !output_file