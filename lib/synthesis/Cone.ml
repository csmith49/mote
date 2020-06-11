(* required types *)
type t = Delta.t list
type heuristic = Delta.t -> int

(* consistency check/transformation for deltas *)
let check_delta
    ?max_nodes:(max_nodes=4)
    ?max_edges:(max_edges=4)
    delta = CCOpt.Infix.(Some delta
        >>= Constraint.keep_selector
        >>= Constraint.max_nodes max_nodes
        >>= Constraint.max_edges max_edges
        >>= Constraint.drop_dangling_edges
        >>= Constraint.stay_connected
        >>= Constraint.attribute_per_node
    )

(* for searching some deltas *)
module DeltaHeap = CCHeap.Make(struct
    type t = Delta.t
    let leq = Delta.PartialOrder.leq
end)

(* construction *)
let from_motifs motifs = motifs
    |> Motifs.Motif.PartialOrder.weak_join
    |> CCList.map Delta.initial

(* sampling *)
let sample_one_change max_nodes max_edges delta =
    let options = delta
        |> Delta.refine
        |> CCList.filter_map (check_delta ~max_nodes:max_nodes ~max_edges:max_edges) in
    match options with
        | [] -> None
        | xs -> Some (CCRandom.run (CCRandom.pick_list xs))

let rec sample_no_backtrack max_nodes max_edges delta =
    if Delta.is_total delta then Some delta else
    match sample_one_change max_nodes max_edges delta with
        | None -> None
        | Some result -> sample_no_backtrack max_nodes max_edges result

let sample_from_cone cone = if CCList.is_empty cone then None else
    Some (CCRandom.run (cone |> CCRandom.pick_list))

let sample
    ?count:(count=1)
    ?verbose:(verbose=false)
    ?max_nodes:(max_nodes=10)
    ?max_edges:(max_edges=10)
        cone =
    
    (* for printing *)
    let vprint = if verbose then print_endline else (fun _ -> ()) in

    (* we only have a list of solutions we maintain *)
    let solutions = ref [] in

    (* until we have enough solutions, keep getting a new one *)
    while (CCList.length !solutions) < count do
        (* nice printing *)
        let _ = vprint (Printf.sprintf "Starting iteration, %d solutions found" (CCList.length !solutions)) in
        (* get something from the heap *)
        let sample = CCOpt.Infix.(
            sample_from_cone cone >>= sample_no_backtrack max_nodes max_edges
        ) in match sample with
            | Some delta -> 
                let motif = Delta.concretize delta in
                solutions := motif :: !solutions
            | None -> ()
    done;
    !solutions

(* basic enumeration *)
let enumerate 
    ?filter:(filter=(fun _ -> true))
    ?verbose:(verbose=false)
        cone =
    
    let vprint = if verbose then print_endline else (fun _ -> ()) in

    (* build the in-place resources *)
    let heap = ref (DeltaHeap.of_list cone) in
    let solutions = ref [] in
    let count = ref 0 in

    (* enumerate until heap is empty *)
    while not (DeltaHeap.is_empty !heap) do
        let _ = vprint (Printf.sprintf "\n[ITERATION %d]" !count) in
        let _ = count := 1 + !count in
        (* how big is the heap *)
        let _ = vprint (Printf.sprintf "[HEAP SIZE] %d" (!heap |> DeltaHeap.size)) in
        (* get the smallest element *)
        let heap', delta = DeltaHeap.take_exn !heap in
        (* print the motif *)
        let _ = vprint (Printf.sprintf "[BEGIN MOTIF]\n%s\n[END MOTIF]" (
            delta |> Delta.concretize |> Motifs.Motif.to_string
        )) in
        (* transform with checks *)
        let delta = check_delta delta in
        match delta with
            | None -> 
                let _ = vprint "[CONSTRAINTS FAILED]" in
                heap := heap'
            | Some delta ->
                let _ = vprint "[CONSTRAINTS PASSED]" in
                (* apply the filter *)
                let filter_result = filter delta in
                (* print if we've passed the filter *)
                (* let _ = vprint (Printf.sprintf "[FILTER CHECK] %b" filter_result) in *)
                let _ = if not filter_result then
                    heap := heap' else
                (* check if it's total, aka can be returned *)
                let is_total = Delta.is_total delta in
                let _ = vprint (Printf.sprintf "[TOTAL?] %b" is_total) in
                let _ = if is_total then
                    solutions := delta :: !solutions in
                (* generate refinements *)
                let refinements = Delta.refine delta in
                let _ = vprint (Printf.sprintf "[REFINEMENTS FOUND] %d" (CCList.length refinements)) in
                (* reconstruct heap *)
                heap := DeltaHeap.add_list heap' refinements
        in vprint "[ITERATION END]"
    done;

    (* return solutions *)
    !solutions |> CCList.rev |> CCList.map Delta.concretize
        |> CCList.uniq ~eq:Motifs.Motif.PartialOrder.equal

(* version of a beam search *)
module BeamFrontier = struct
    module BeamHeap = CCHeap.Make(struct
        type t = int * Delta.t
        let leq x y = match x, y with
            | (ix, _), (iy, _) -> ix <= iy
    end)

    type t = {
        heap : BeamHeap.t;
        size : int;
        heuristic : heuristic;
        max_width : int;
    }

    let init width heuristic = {
        heap = BeamHeap.empty;
        size = 0;
        heuristic = heuristic;
        max_width = width;
    }

    let add frontier delta =
        let item = (frontier.heuristic delta, delta) in
        if (frontier.size + 1) > frontier.max_width then
            { frontier with
                heap = item
                    |> BeamHeap.add frontier.heap
                    |> BeamHeap.take_exn
                    |> fst
            }
        else
            { frontier with
                heap = item |> BeamHeap.add frontier.heap;
                size = frontier.size + 1;
            }

    let to_list frontier = frontier.heap
        |> BeamHeap.to_list
        |> CCList.map snd
    let of_list width heuristic deltas =
        let initial = init width heuristic in
        CCList.fold_left add initial deltas
end

let beam_search 
    ?width:(width=100)
    ?heuristic:(heuristic=fun _ -> 0)
    ?verbose:(verbose=false)
        cone =
    
    let vprint = if verbose then print_endline else (fun _ -> ()) in

    (* build the in-place resources *)
    let cone = ref cone in
    let solutions = ref [] in

    while not (CCList.is_empty !cone) do
        (* split the cone into total and not deltas *)
        let total_deltas, extendable_deltas = !cone
            |> CCList.partition Delta.is_total in
        let _ = vprint ("Found " ^ (total_deltas |> CCList.length |> string_of_int) ^ " total solutions.") in
        (* record the totals as solutions *)
        let _ = solutions := total_deltas @ !solutions in
        (* extend the not totals *)
        let refinements = extendable_deltas
            |> CCList.flat_map Delta.refine
            |> CCList.filter_map check_delta
            |> BeamFrontier.of_list width heuristic in
        (* trim the refinements to size *)
        cone := refinements |> BeamFrontier.to_list
    done;

    (* return solutions *)
    !solutions |> CCList.rev |> CCList.map Delta.concretize
        |> CCList.uniq ~eq:Motifs.Motif.PartialOrder.equal