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
let files =
    let file_refs = ref [] in
    let json_stream = Yojson.Basic.linestream_from_file !data_file in
    let _ = Stream.iter (fun json_line ->
        let filename = match json_line with
            | `Json json -> Utility.JSON.string json
            | _ -> None
        in match filename with
            | Some filename -> file_refs := filename :: !file_refs
            | None -> ()
    ) json_stream in
    !file_refs
let num_files = CCList.length files
let _ = print_endline ("Done. Found " ^ (num_files |> string_of_int) ^ " data files.")

(* loading the motifs *)
let _ = print_endline "Loading input motifs..."
let motifs =
    let motif_refs = ref [] in
    let json_stream = Yojson.Basic.linestream_from_file !motif_file in
    let _ = Stream.iter (fun json_line ->
        let motif = match json_line with
            | `Json json -> Motifs.Motif.of_json json
            | _ -> None
        in match motif with
            | Some motif -> motif_refs := motif :: !motif_refs
            | None -> ()
    ) json_stream in
    !motif_refs
let _ = print_endline ("Done. Found " ^ (motifs |> CCList.length |> string_of_int) ^ " motifs.")


(* now build the sparse image *)
let _ = print_endline "Building image..."
let sparse_image = ref (Domain.SparseImage.of_motifs motifs)
let _ = CCList.iteri (fun i -> fun filename ->
    let _ = Printf.printf "Evaluating %s (%d/%d)\n%!" filename i num_files in
    let db = Domain.SQL.of_string filename in
    let images = CCList.map (Domain.SQL.evaluate db) motifs in
    let _ = Domain.SQL.close db in
    sparse_image := Domain.SparseImage.add_results filename images !sparse_image
) files
let _ = Yojson.Basic.to_file !output_file (Domain.SparseImage.to_json !sparse_image)
let _ = Printf.printf "...done. Output written to %s\n" !output_file