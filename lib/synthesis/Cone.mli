type t

type heuristic = Delta.t -> int

val from_motifs : Motifs.Motif.t list -> t

val sample : ?count:(int) -> ?verbose:(bool) -> ?max_nodes:(int) -> ?max_edges:(int) -> t -> Motifs.Motif.t list
val enumerate : ?filter:(Delta.t -> bool) -> ?verbose:(bool) -> t -> Motifs.Motif.t list
val beam_search : ?width:(int) -> ?heuristic:(heuristic) -> ?verbose:(bool) -> t -> Motifs.Motif.t list