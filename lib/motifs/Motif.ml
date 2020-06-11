type pattern_graph = (Filter.t, Kinder.t) Core.Structure.t

type t = {
    selector : Core.Identifier.t;
    structure : pattern_graph;
}

let pg_to_json = Core.Structure.to_json Filter.to_json Kinder.to_json
let pg_of_json = Core.Structure.of_json 
    Filter.of_json Filter.empty
    Kinder.of_json Kinder.Star

let to_json motif = `Assoc [
    ("selector", Core.Identifier.to_json motif.selector);
    ("structure", pg_to_json motif.structure)
]
let of_json json =
    let selector = Utility.JSON.get "selector" Core.Identifier.of_json json in
    let structure = Utility.JSON.get "structure" pg_of_json json in
    match selector, structure with
        | Some selector, Some structure -> Some {
            selector = selector ; structure = structure
        }
        | _ -> None

let hash = CCHash.poly

let to_string motif = 
    let struct_s = Core.Structure.to_string
        Filter.to_string
        Kinder.to_string
        motif.structure in
    let sel_s = Core.Identifier.to_string motif.selector in
    if CCString.is_empty struct_s then Printf.sprintf "select %s in Ø" sel_s else
        Printf.sprintf "select %s in\n%s" sel_s struct_s

(* utility module *)
module U = struct
    let id_mem = CCList.mem ~eq:Core.Identifier.equal
end

let well_connected motif =
    let reachable = Core.Structure.Algorithms.bireachable motif.structure [motif.selector] in
    let vertices = Core.Structure.vertices motif.structure in
    vertices |>
        CCList.for_all (fun v -> U.id_mem v reachable)

let well_formed motif =
    U.id_mem motif.selector (Core.Structure.vertices motif.structure)


(* PARTIAL ORDER *)

module PartialOrder = struct
    (* we're going to be manipulating embeddings a bunch *)
    module E = Core.Structure.Embedding

    (* picks out nearby nodes to be candidates *)
    let candidates motif ids =
        let neighborhood = Core.Structure.Algorithms.neighborhood
            motif.structure ids 1 in
        CCList.filter (fun v -> not (U.id_mem v ids)) neighborhood

    (* check if an edge embeds *)
    let edge_embeds left embedding edge = match E.edge_image edge embedding with
        | None -> false
        | Some (src, lbl, dest) -> Core.Structure.outgoing src left.structure
            (* get all edges that actually go from src to dest *)
            |> CCList.filter (fun e -> Core.Identifier.equal
                dest
                (Core.Structure.Edge.destination e))
            (* remove any that don't satisfy our desired implication ordering *)
            |> CCList.filter (fun e -> Kinder.( (Core.Structure.Edge.label e) => lbl))
            (* make sure that some edge actually exists *)
            |> CCList.is_empty
            |> CCBool.negate
    
    (* check if a vertex embeds *)
    let vertex_embeds left right embedding vertex = match E.image vertex embedding with
        | None -> false
        | Some vertex' -> 
            match Core.Structure.label vertex right.structure, Core.Structure.label vertex' left.structure with
                | Some label, Some label' -> Filter.(label' => label) 
                | _ -> false

    (* generates candidate pairs to extend the embedding : (id, id) list *)
    let candidate_pairs left right embedding =
        let dom_candidates = candidates right (E.domain embedding) in
        let codom_candidates = candidates left (E.codomain embedding) in
        CCList.cartesian_product [dom_candidates ; codom_candidates]
            |> CCList.filter_map (fun ls -> match ls with
                | right :: left :: [] -> Some (right, left)
                | _ -> None)

    (* get edges *)
    let domain_edge_constraints right embedding vertex =
        let domain = E.domain embedding in
        let in_domain v = U.id_mem v domain in
        let incoming = Core.Structure.incoming vertex right.structure
            |> CCList.filter (fun e -> in_domain (Core.Structure.Edge.source e)) in
        let outgoing = Core.Structure.outgoing vertex right.structure
            |> CCList.filter (fun e -> in_domain (Core.Structure.Edge.destination e)) in
        incoming @ outgoing

    (* check if an embedding is total *)
    let is_total right embedding = candidates right (E.domain embedding)
        |> CCList.is_empty

    (* refine an embedding *)
    let refine left right embedding = candidate_pairs left right embedding
        |> CCList.filter_map (fun (r, l) ->
            let emb = E.extend r l embedding in
            if vertex_embeds left right emb r then
                let constraints = domain_edge_constraints right emb r in
                if CCList.for_all (fun e -> edge_embeds left emb e) constraints then
                    Some emb
                else None
            else None
        )

    (* [leq l r] is true if there is an embedding of r into l such that labels respect l => r *)
    let rec leq left right =
        let initial = E.embed right.selector left.selector in
        if vertex_embeds left right initial right.selector then
            leq_aux left right [initial]
        else false
    and leq_aux left right embeddings = match embeddings with
        | [] -> false
        | embedding :: rest -> if is_total right embedding then true else
            let embeddings = (refine left right embedding) @ rest in
            leq_aux left right embeddings

    (* we can generalize this to get all embeddings, as opposed to testing for the existence of 1 *)
    let rec embeddings left right =
        let initial = E.embed right.selector left.selector in
        if vertex_embeds left right initial right.selector then
            embeddings_aux left right [initial]
        else []
    and embeddings_aux left right embeddings = match embeddings with
        | [] -> []
        | embedding :: rest ->
            if is_total right embedding then
                embedding :: (embeddings_aux left right rest)
            else
                let embeddings = (refine left right embedding @ rest) in
                embeddings_aux left right embeddings

    (* alternate syntax *)
    let (<=) x y = leq x y

    (* JOINS *)

    (* computed over candidates *)
    type candidate = {
        c_selector : Core.Identifier.t;
        c_structure : pattern_graph;
        c_embeddings : E.t list;
    }

    (* extensions are lists of images *)
    type extension = {
        domain : Core.Identifier.t;
        codomains : Core.Identifier.t list;
    }

    (* convert to motif *)
    let to_motif (candidate : candidate) : t = {
        selector = candidate.c_selector;
        structure = candidate.c_structure;
    }

    (* consistent if this is the only way we make candidates *)
    let fresh_id (candidate : candidate) : Core.Identifier.t = candidate.c_structure
        |> Core.Structure.vertices
        |> CCList.length
        |> Core.Identifier.of_int

    (* get an initial candidate by "intersecting" all the selectors *)
    let initial_candidate (motifs : t list) : candidate =
        let selector = Core.Identifier.of_int 0 in
        let label = motifs
            |> CCList.filter_map (fun m -> Core.Structure.label m.selector m.structure)
            |> Filter.Lattice.join in
        let structure = Core.Structure.empty
            |> Core.Structure.add_vertex selector label in
        let embeddings = motifs
            |> CCList.map (fun m -> E.embed selector m.selector) in
        {
            c_selector = selector;
            c_structure = structure;
            c_embeddings = embeddings;
        }

    (* find all candidate extensions by looking in the neighborhood of each codomain *)
    let extensions (motifs : t list) (candidate : candidate) : extension list =
        let codomains = candidate.c_embeddings |> CCList.map E.codomain in
        let candidates = codomains |> CCList.map2 (fun m -> fun codom ->
            Core.Structure.Algorithms.neighborhood m.structure codom 1
                |> CCList.filter (fun n -> not (U.id_mem n codom))
        ) motifs in
        candidates |> CCList.cartesian_product |> CCList.map (fun e -> 
            { domain=fresh_id candidate ; codomains=e }
        )

    (* given an extension, compute the strongest possible label in the join *)
    let extension_label (motifs : t list) (extension : extension) : Filter.t =
        CCList.map2 (fun m -> fun x -> Core.Structure.label x m.structure) motifs extension.codomains
            |> CCList.all_some
            |> CCOpt.get_or ~default:[]
            |> Filter.Lattice.join
    
    (* given a set of edges, see if they're all equal *)
    let rec edge_join : Kinder.t Core.Structure.edge list -> Kinder.t Core.Structure.edge option = function
        | [] -> None
        | e :: [] -> Some e
        | e :: rest -> match edge_join rest with
            | Some e' -> if Core.Structure.Edge.equal Kinder.equal e e' then Some e else None
            | None -> None

    (* get any edges to be added to the candidate *)
    let edges_from_extension
        (motifs : t list)
        (candidate : candidate)
        (extension : extension) : Kinder.t Core.Structure.edge list =
        let open Core.Structure in
        (* gets edges in preimage *)
        let edges_from_motif motif embedding candidate =
            let embedding' = E.extend candidate extension.domain embedding in
            let allowed_nodes = E.codomain embedding' in
            let incoming_edges = incoming candidate motif.structure
                (* get the edges from allowed to candidate *)
                |> CCList.filter (fun e -> U.id_mem (Edge.source e) allowed_nodes)
                (* and convert them to the preimage *)
                |> CCList.filter_map (fun e -> match E.preimage (Edge.source e) embedding' with
                    | Some src -> Some (src, Edge.label e, extension.domain)
                    | None -> None
                ) in
            let outgoing_edges = outgoing candidate motif.structure
                (* get the nodes from candidate to allowed *)
                |> CCList.filter (fun e -> U.id_mem (Edge.destination e) allowed_nodes)
                (* convert *)
                |> CCList.filter_map (fun e -> match E.preimage (Edge.destination e) embedding' with
                    | Some dest -> Some (extension.domain, Edge.label e, dest)
                    | None -> None) in
            incoming_edges @ outgoing_edges in
        (* get each "problem instance" *)
        let problems = CCList.map2 (fun m -> fun e -> (m, e)) motifs candidate.c_embeddings
            |> CCList.map2 (fun candidate -> fun (m, e) -> (m, e, candidate)) extension.codomains in
        let edges_per_motif = CCList.map (fun (m, e, c) -> edges_from_motif m e c) problems in
        edges_per_motif
            |> CCList.cartesian_product
            |> CCList.filter_map edge_join

    (* apply an extension *)
    let apply_extension (motifs : t list) (candidate : candidate) (extension : extension) : candidate option =
        let label = extension_label motifs extension in
        let edges = edges_from_extension motifs candidate extension in
        let new_structure = candidate.c_structure
            |> Core.Structure.add_vertex extension.domain label
            |> CCList.fold_right Core.Structure.add_edge edges in
        let embeddings = candidate.c_embeddings
            |> CCList.map2 (fun c -> fun e -> E.extend extension.domain c e) extension.codomains in
        match edges with
            | [] -> None
            | _ -> Some {
                candidate with c_structure = new_structure ; c_embeddings = embeddings
            }

    let refine_candidate (motifs : t list) (candidate : candidate) : candidate list =
        let extensions = extensions motifs candidate in
        extensions |> CCList.filter_map (fun ext ->
            apply_extension motifs candidate ext
        )
    
    (* apply a step - if we can't refine any more, we're already maximal, so that's good *)
    let step motifs candidate = match refine_candidate motifs candidate with
        | [] -> `Maximal
        | _ as ans -> `Refinements ans

    (* this is verrrrrrry slow *)
    let join motifs = match motifs with
        | [] -> []
        | x :: [] -> [x]
        | motifs ->
            let candidates = ref [initial_candidate motifs] in
            let answers = ref [] in
            while not (CCList.is_empty !candidates) do
                let candidate, rest = CCList.hd_tl !candidates in
                match step motifs candidate with
                    | `Maximal -> begin
                        answers := candidate :: !answers;
                        candidates := rest
                    end
                    | `Refinements refs -> begin
                        candidates := rest @ refs;
                    end
            done;
            !answers |> CCList.map to_motif


    (* equality *)
    let equal left right = leq left right && leq right left

    (* this is a quicker approximation, assumes all graphs are identical a certain distance away from the selector *)
    let drop_all_but motif ids = 
        let drop = motif.structure
            |> Core.Structure.vertices
            |> CCList.filter (fun v -> not (U.id_mem v ids)) in
        {
            motif with structure = CCList.fold_right Core.Structure.remove_vertex drop motif.structure
        }
    let submotif motif size =
        let neighborhood = Core.Structure.Algorithms.neighborhood motif.structure [motif.selector] size in
            drop_all_but motif neighborhood
    let replace_submotif motif neighborhood submotif =
        let open Core.Structure in
        let subvertices = submotif.structure
            |> vertices
            |> CCList.filter_map (fun v ->
                match label v submotif.structure with
                    | Some lbl -> Some (v, lbl)
                    | None -> None) in
        let comp_vertices = motif.structure
            |> vertices
            |> CCList.filter_map (fun v ->
                if U.id_mem v neighborhood then None else
                match label v motif.structure with
                    | Some lbl -> Some (v, lbl)
                    | None -> None) in
        let subedges = submotif.structure |> edges in
        let complement = comp_vertices |> CCList.map fst in
        let comp_edges = motif.structure
            |> edges
            |> CCList.filter (fun e ->
                (* case 1 - we're entirely in the complement *)
                let s, d = Edge.source e, Edge.destination e in
                if U.id_mem s complement && U.id_mem d complement then true else
                (* case 2 - src in comp, dest in submotif *)
                if U.id_mem s complement && U.id_mem d (subvertices |> CCList.map fst) then true else
                (* case 2.1 - symmetric to above *)
                if U.id_mem d complement && U.id_mem s (subvertices |> CCList.map fst) then true else
                false
            ) in
        let structure = empty
            |> CCList.fold_right (fun (v, lbl) -> fun s -> add_vertex v lbl s) (subvertices @ comp_vertices)
            |> CCList.fold_right add_edge (subedges @ comp_edges) in
        { selector = submotif.selector ; structure = structure }

    let embed_motif motif embedding = 
        let selector = E.image motif.selector embedding in 
        let structure = E.embed_structure motif.structure embedding in
        match selector, structure with
            | Some selector, Some structure -> Some { selector = selector ; structure = structure}
            | _ -> None
    let weak_join motifs = match motifs with
        | [] -> []
        | x :: [] -> [x]
        | (motif :: _) as motifs ->
            let small_motifs = CCList.map (fun m -> submotif m 1) motifs in
            (* neighborhood for reconstruction *)
            let neighborhood = small_motifs
                |> CCList.flat_map (fun m -> m.structure |> Core.Structure.vertices)
                |> CCList.uniq ~eq:Core.Identifier.equal in
            let small_joins = join small_motifs |> CCList.uniq ~eq:equal in
            (* embed the small joins into the head motif *)
            let embedded_joins = small_joins
                |> CCList.flat_map (fun j -> j |> embeddings motif |> CCList.filter_map (embed_motif j))
                |> CCList.uniq ~eq:equal in
            (* replace the small motif with the embedded joins *)
            CCList.map (replace_submotif motif neighborhood) embedded_joins
end