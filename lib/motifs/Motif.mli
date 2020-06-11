(** pattern graphs store filters and kinders on vertices and edges *)
type pattern_graph = (Filter.t, Kinder.t) Core.Structure.t

(** motifs select a particular node in a pattern graph *)
type t = {
    selector : Core.Identifier.t;
    structure : pattern_graph;
}

(* * hash a motif *)
val hash : t -> int

(** convert a motif to a json representation *)
val to_json : t -> Yojson.Basic.t

(* * reconstruct a motif from a json representation *)
val of_json : Yojson.Basic.t -> t option

(* * convert a motif to a string suitable for printing *)
val to_string : t -> string

(** check if a motif is well-connected *)
val well_connected : t -> bool

val well_formed : t -> bool

(** motifs form a natural partial order from subset inclusion on the image *)
module PartialOrder : sig
    val equal : t -> t -> bool
    val leq : t -> t -> bool
    val (<=) : t -> t -> bool

    (** to implement join, must lift to lattice of antichains *)
    val join : t list -> t list
    val weak_join : t list -> t list
end