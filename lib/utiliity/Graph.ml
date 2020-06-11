module type VERTEX = sig
    type t
    val compare : t -> t -> int
    val equal : t -> t -> bool
end

module type GRAPH = sig
    type vertex
    type 'e edge = vertex * 'e * vertex
    type ('v, 'e) t

    (* access *)
    val mem : vertex -> ('v, 'e) t -> bool
    val incoming : vertex -> ('v, 'e) t -> ('e edge) list
    val outgoing : vertex -> ('v, 'e) t -> ('e edge) list
    val label : vertex -> ('v, 'e) t -> 'v option
    val vertices : ('v, 'e) t -> vertex list
    val edges : ('v, 'e) t -> ('e edge) list

    (* construction *)
    val empty : ('v, 'e) t
    val add_edge : 'e edge -> ('v, 'e) t -> ('v, 'e) t
    val add_vertex : vertex -> 'v -> ('v, 'e) t -> ('v, 'e) t
    val add_label : vertex -> 'v -> ('v, 'e) t -> ('v, 'e) t
    val remove_edge : 
        eq:('e -> 'e -> bool) -> 'e edge -> ('v, 'e) t -> ('v, 'e) t
    val remove_vertex : vertex -> ('v, 'e) t -> ('v, 'e) t

    (* manipulation *)
    val map : ('v -> 'u) -> ('e -> 'g) -> ('v, 'e) t -> ('u, 'g) t
end

module Make (V : VERTEX) : (GRAPH with type vertex = V.t) = struct
    type vertex = V.t
    module VertexMap = CCMap.Make(V)
    type 'e edge = vertex * 'e * vertex
    type ('v, 'e) context = {
        label : 'v;
        incoming : ('e edge) list;
        outgoing : ('e edge) list;
    }
    type ('v, 'e) t = ('v, 'e) context VertexMap.t

    (* access *)
    let mem vertex graph = VertexMap.mem vertex graph
    let context vertex graph = VertexMap.find_opt vertex graph

    let incoming vertex graph = match context vertex graph with
        | Some context -> context.incoming
        | None -> []
    let outgoing vertex graph = match context vertex graph with
        | Some context -> context.outgoing
        | None -> []

    let label vertex graph = match context vertex graph with
        | Some context -> Some context.label
        | None -> None

    let vertices graph = graph
        |> VertexMap.to_list
        |> CCList.map fst

    let edges graph = graph
        |> vertices
        |> CCList.flat_map (fun v -> outgoing v graph)

    (* construction *)
    let empty = VertexMap.empty

    let add_vertex vertex label graph =
        let context = {
            label = label;
            incoming = [];
            outgoing = [];
        } in VertexMap.add vertex context graph

    let add_edge edge graph =
        let src, _, dest = edge in
        match context src graph, context dest graph with
            | Some src_c, Some dest_c -> graph
                |> VertexMap.add 
                    src
                    {src_c with outgoing = edge :: src_c.outgoing}
                |> VertexMap.add
                    dest
                    {dest_c with incoming = edge :: dest_c.incoming}
            | _ -> graph

    let add_label vertex label graph = match context vertex graph with
        | Some c -> VertexMap.add vertex {c with label = label} graph
        | None -> add_vertex vertex label graph

    let src (src, _, _) = src
    let dest (_, _, dest) = dest
    let remove_vertex vertex graph = graph
        |> VertexMap.remove vertex
        |> VertexMap.to_list
        |> CCList.map (fun (v, c) ->
            let out = c.outgoing
                |> CCList.filter (fun e -> not (V.equal (dest e) v)) in
            let inc = c.incoming
                |> CCList.filter (fun e -> not (V.equal (src e) v)) in
            let context = {c with outgoing = out; incoming = inc;} in
            (v, context))
        |> VertexMap.of_list

    let edge_eq (eq : 'e -> 'e -> bool) (e1 : 'e edge) (e2 : 'e edge) =
        let s1, l1, d1 = e1 in let s2, l2, d2 = e2 in
            (V.equal s1 s2) && (V.equal d1 d2) && (eq l1 l2)

    let remove_edge 
        ~(eq : 'e -> 'e -> bool) (edge : 'e edge) (graph : ('v, 'e) t) =
        let src, _, dest = edge in
        let graph = match context src graph with
            | Some context ->
                let out = context.outgoing
                    |> CCList.remove_one ~eq:(edge_eq eq) edge in
                let context = {context with outgoing = out;} in
                VertexMap.add src context graph
            | None -> graph in
        match context dest graph with
            | Some context ->
                let inc = context.incoming
                    |> CCList.remove_one ~eq:(edge_eq eq) edge in
                let context = {context with incoming = inc;} in
                VertexMap.add dest context graph
            | None -> graph

    (* mapping *)
    let map vertex_label_map edge_label_map graph =
        let edge_map edge = match edge with
            | (src, lbl, dest) -> (src, edge_label_map lbl, dest) in
        let context_map context = {
            label = vertex_label_map context.label;
            incoming = CCList.map edge_map context.incoming;
            outgoing = CCList.map edge_map context.outgoing;
        } in graph
            |> VertexMap.to_list
            |> CCList.map (CCPair.map2 context_map)
            |> VertexMap.of_list
end