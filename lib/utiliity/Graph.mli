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

    val map : ('v -> 'u) -> ('e -> 'g) -> ('v, 'e) t -> ('u, 'g) t
end

module Make (V : VERTEX) : GRAPH
    with type vertex = V.t