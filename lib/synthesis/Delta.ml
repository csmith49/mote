type change = [
    | `Keep
    | `Remove
    | `Weaken of Motifs.Filter.t
]

let change_leq left right = match left, right with
    | `Keep, _ -> true
    | _, `Remove -> true
    | `Weaken f, `Weaken g -> Motifs.Filter.(f => g)
    | _ -> false
let change_eq left right = match left, right with
    | `Keep, `Keep -> true
    | `Remove, `Remove -> true
    | `Weaken f, `Weaken g -> Motifs.Filter.equal f g
    | _ -> false

(* let change_to_string = function
    | `Keep -> "KEEP"
    | `Remove -> "REMOVE"
    | `Weaken f -> Printf.sprintf "WEAKEN %s" (Matcher.Filter.to_string f) *)

type vertex = Core.Identifier.t
type edge = vertex * Motifs.Kinder.t * vertex

type index =
    | V of vertex
    | E of edge
let index_eq left right = match left, right with
    | V v, V u -> Core.Identifier.equal v u
    | E e, E h -> Core.Structure.Edge.equal    
        Motifs.Kinder.equal e h
    | _ -> false


type t = {
    base_motif : Motifs.Motif.t;
    motif_hash : int;
    changes : (index * change) list;
}

let changes delta = delta.changes
let motif delta = delta.base_motif

let apply_change structure (index, change) = match index, change with
    | V v, `Remove -> Core.Structure.remove_vertex v structure
    | E e, `Remove -> Core.Structure.remove_edge ~eq:Motifs.Kinder.equal e structure
    | V v, `Weaken f -> Core.Structure.add_label v f structure
    | _ -> structure (* we ignore any other change *)

let concretize delta =
    let structure = CCList.fold_left apply_change delta.base_motif.structure delta.changes in
    {
        delta.base_motif with structure = structure;
    }

let initial motif = {
    base_motif = motif;
    motif_hash = Motifs.Motif.hash motif;
    changes = [];
}

let indices_of_motif motif = 
    let structure = motif.Motifs.Motif.structure in
    let edge_indices = structure
        |> Core.Structure.edges
        |> CCList.map (fun e -> E e) in
    let vertex_indices = structure
        |> Core.Structure.vertices
        |> CCList.map (fun v -> V v) in
    vertex_indices @ edge_indices
let indices delta = delta.changes
    |> CCList.map fst
let free_indices delta =
    let bound_indices = indices delta in
    delta.base_motif
        |> indices_of_motif
        |> CCList.filter (fun i ->
            not (CCList.mem ~eq:index_eq i bound_indices)
        )

let is_total delta = delta
    |> free_indices
    |> CCList.is_empty

let free_index delta = delta
    |> free_indices
    |> CCList.head_opt

let label vertex delta = Core.Structure.label
    vertex
    delta.base_motif.Motifs.Motif.structure

let add_change delta change =
    let i, c = change in
    match CCList.assoc_opt ~eq:index_eq i delta.changes with
        | Some c' -> if change_eq c c' then Some delta else None
        | None -> Some {delta with changes = change :: delta.changes}
let rec add_changes delta changes = match changes with
    | [] -> Some delta
    | c :: rest -> match add_changes delta rest with
        | Some d -> add_change d c
        | None -> None

let refine delta = match free_index delta with
    | Some (V v) ->
        let filters = match label v delta with
            | Some f -> Motifs.Filter.Lattice.weaken f
            | None -> [] in
        let weakened = CCList.map (fun f -> `Weaken f) filters in
        let changes = `Keep :: `Remove :: weakened
            |> CCList.map (fun c -> (V v, c)) in
        CCList.filter_map (add_change delta) changes
    | Some (E e) ->
        let changes = `Keep :: `Remove :: []
            |> CCList.map (fun c -> (E e, c)) in
        CCList.filter_map (add_change delta) changes
    | None -> []

module PartialOrder = struct
    let entry_eq = CCPair.equal index_eq change_eq

    (* for partial order heaps *)
    let lift_comparison change_cmp left right =
        (* make sure we're dealing with the same motif by checking the hash *)
        if left.motif_hash != right.motif_hash then false else
        (* if we are, we have to check if each delta is comparable *)
        CCList.for_all (fun l -> 
            CCList.exists (fun r -> 
                (CCPair.equal index_eq change_cmp) l r)
            right.changes)
        left.changes

    (* constructing comparisons *)
    let equal = lift_comparison change_eq
    let leq = lift_comparison change_leq
end
