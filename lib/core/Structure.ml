(* graph includes *)

module Graph = Utility.Graph.Make(Identifier)

include Graph

(* type aliases *)

type ('v, 'e) structure = ('v, 'e) t

(* JSON parsing *)

let of_json j2v dv j2e de json =
    let j2edge json =
        let src = Utility.JSON.get "source" Identifier.of_json json in
        let lbl = Utility.JSON.get "label" j2e json in
        let dest = Utility.JSON.get "destination" Identifier.of_json json in
        match src, lbl, dest with
            | Some src, Some lbl, Some dest -> Some (src, lbl, dest)
            | Some src, None, Some dest -> Some (src, de, dest)
            | _ -> None in
    let j2vertex json =
        let id = Utility.JSON.get "identifier" Identifier.of_json json in
        let lbl = Utility.JSON.get "label" j2v json in
        match id, lbl with
            | Some id, Some lbl -> Some (id, lbl)
            | Some id, None -> Some (id, dv)
            | _ -> None in
    let vertices = json
        |> Utility.JSON.get "vertices" (Utility.JSON.list j2vertex) in
    let edges = json
        |> Utility.JSON.get "edges" (Utility.JSON.list j2edge) in
    match vertices, edges with
        | Some vertices, Some edges -> empty
            |> CCList.fold_right (fun (v, l) -> add_vertex v l) vertices
            |> CCList.fold_right add_edge edges
            |> CCOpt.return
        | _ -> None

let to_json (v2j : 'v -> Yojson.Basic.t) (e2j : 'e -> Yojson.Basic.t) (g : ('v, 'e) t) : Yojson.Basic.t =
    let vertices = g
        |> vertices
        |> CCList.filter_map (fun v -> match label v g with
            | Some lbl -> `Assoc [
                ("identifier", Identifier.to_json v);
                ("label", v2j lbl)
            ] |> CCOpt.return
            | None -> None) in
    let edges = g
        |> edges
        |> CCList.map (fun (src, lbl, dest) -> `Assoc [
            ("source", Identifier.to_json src);
            ("label", e2j lbl);
            ("destination", Identifier.to_json dest)
        ]) in
    `Assoc [
        ("edges", `List edges);
        ("vertices", `List vertices)
    ]

(* Edge utility module *)

module Edge = struct
    let equal eq left right = match left, right with
        | (src, lbl, dest), (src', lbl', dest') ->
            (Identifier.equal src src') &&
            (Identifier.equal dest dest') &&
            (eq lbl lbl')

    let compare cmp left right = match left, right with
        | (src, lbl, dest), (src', lbl', dest') ->
            let src_cmp = Identifier.compare src src' in
            if src_cmp = 0 then
                let lbl_cmp = cmp lbl lbl' in
                if lbl_cmp = 0 then
                    Identifier.compare dest dest'
                else lbl_cmp
            else src_cmp

    let source (src, _, _) = src
    let destination (_, _, dest) = dest
    let label (_, lbl, _) = lbl
end

(* Bipath utility module *)
module BiPath = struct
    type 'e t = {
        source : Identifier.t;
        destination : Identifier.t;
        edges : 'e edge list;
    }

    let of_forward_edge edge = {
        source = Edge.source edge;
        destination = Edge.destination edge;
        edges = [edge];
    }
    let of_backward_edge edge = {
        source = Edge.destination edge;
        destination = Edge.source edge;
        edges = [edge];
    }

    let extend bipath edge =
        if Identifier.equal bipath.destination (Edge.source edge) then
            Some { bipath with
                destination = Edge.destination edge;
                edges = bipath.edges @ [edge]}
        else if Identifier.equal bipath.destination (Edge.destination edge) then
            Some { bipath with
                destination = Edge.source edge;
                edges = bipath.edges @ [edge]}
        else None
    
    let edges bipath = bipath.edges
    let source bipath = bipath.source
    let destination bipath = bipath.destination

    let get_next src edge =
        if Identifier.equal src (Edge.source edge) then
            Edge.destination edge
        else Edge.source edge
    let rec visited bipath = visited_aux bipath.source bipath.edges
    and visited_aux src edges = match edges with
        | [] -> [src]
        | e :: rest -> src :: (visited_aux (get_next src e) rest)
    let loop_free bipath =
        let states = visited bipath in
        let num_states = CCList.length states in
        let num_unique_states = states 
            |> CCList.uniq ~eq:Identifier.equal |> CCList.length in
        num_states = num_unique_states

    let between structure src dest =
        (* initialize worklist *)
        let out_paths = outgoing src structure
            |> CCList.map of_forward_edge in
        let in_paths = incoming src structure
            |> CCList.map of_backward_edge in
        (* mainained resources *)
        let worklist = ref (out_paths @ in_paths) in
        let output = ref [] in
        while not (CCList.is_empty !worklist) do
            let bipath, rest = CCList.hd_tl !worklist in
            if Identifier.equal (destination bipath) (dest) then
                output := bipath :: !output
            else
                let src = destination bipath in
                let out_paths = outgoing src structure
                    |> CCList.filter_map (extend bipath) in
                let in_paths = incoming src structure
                    |> CCList.filter_map (extend bipath) in
                let loop_free_paths = CCList.filter 
                    loop_free
                    (out_paths @ in_paths) in
                worklist := rest @ loop_free_paths
        done;
        !output
end

(* string conversion *)

let to_string v2s e2s structure =
    let context_strings = structure |> vertices
        |> CCList.map (fun v ->
            let v_label = match label v structure with
                | Some vl -> Printf.sprintf
                    " @ %s" (v2s vl)
                | None -> "" in
            let edges = outgoing v structure
                |> CCList.map (fun (_, lbl, dest) -> Printf.sprintf
                    "  --{%s}-> %s" (e2s lbl) (Identifier.to_string dest)
                ) in
            (* if there aren't any edges... *)
            if CCList.is_empty edges then Printf.sprintf "%s%s"
                (Identifier.to_string v)
                (v_label)
            else Printf.sprintf "%s%s\n%s"
                (Identifier.to_string v)
                (v_label)
                (edges |> CCString.concat "\n")
        ) in
    context_strings |> CCString.concat "\n"

module Algorithms = struct
    module IdSet = CCSet.Make(Identifier)
    
    let step structure id = structure
        |> outgoing id
        |> CCList.map Edge.destination
        |> IdSet.of_list
    let bistep structure id = structure
        |> incoming id
        |> CCList.map Edge.source
        |> IdSet.of_list
        |> IdSet.union (step structure id)

    let extend structure ids = IdSet.fold
        (fun v -> fun g -> IdSet.union g (step structure v))
        ids ids
    let biextend structure ids = IdSet.fold
        (fun v -> fun g -> IdSet.union g (bistep structure v))
        ids ids

    let nearby structure vertex =
        let incoming = incoming vertex structure
            |> CCList.map Edge.source in
        let outgoing = outgoing vertex structure
            |> CCList.map Edge.destination in
        CCList.uniq ~eq:Identifier.equal (incoming @ outgoing)

    let rec neighborhood structure ids size =
        if size <= 0 then ids else
        let bigger = CCList.flat_map (nearby structure) ids
            |> CCList.append ids
            |> CCList.uniq ~eq:Identifier.equal in
        neighborhood structure bigger (size - 1)

    let rec reachable structure ids =
        let set = IdSet.of_list ids in
        reachable_aux structure set |> IdSet.to_list
    and reachable_aux structure ids =
        let extended = extend structure ids in
        if IdSet.equal ids extended then ids else reachable_aux structure extended
    
    let rec bireachable structure ids =
        let set = IdSet.of_list ids in
        bireachable_aux structure set |> IdSet.to_list
    and bireachable_aux structure ids =
        let biextended = biextend structure ids in
        if IdSet.equal ids biextended then ids else bireachable_aux structure biextended
end

module Embedding = struct
    module IdentBijection = CCBijection.Make(Identifier)(Identifier)

    type t = IdentBijection.t

    let domain embedding = embedding
        |> IdentBijection.to_list
        |> CCList.map fst
    let codomain embedding = embedding
        |> IdentBijection.to_list
        |> CCList.map snd
    
    let empty = IdentBijection.empty
    let extend left right embedding = IdentBijection.add left right embedding
    let embed left right = extend left right empty
    let image left embedding = 
        try Some (IdentBijection.find_left left embedding)
        with Not_found -> None
    let preimage right embedding =
        try Some (IdentBijection.find_right right embedding)
        with Not_found -> None

    let edge_image (src, lbl, dest) embedding =
        match image src embedding, image dest embedding with
            | Some s, Some d -> Some (s, lbl, d)
            | _ -> None

    let embed_structure structure embedding =
        let vertices = structure
            |> vertices
            |> CCList.map (fun v -> match image v embedding, label v structure with
                | Some v, Some lbl -> Some (v, lbl)
                | _ -> None)
            |> CCList.all_some in
        let edges = structure
            |> edges
            |> CCList.map (fun e -> edge_image e embedding)
            |> CCList.all_some in
        match vertices, edges with
            | Some vertices, Some edges ->
                Graph.empty
                    |> CCList.fold_right (fun (v, lbl) -> fun s ->
                            Graph.add_vertex v lbl s
                        ) vertices
                    |> CCList.fold_right Graph.add_edge edges
                    |> CCOpt.return
            | _ -> None
end