type filename = string
type image = filename * (Core.Identifier.t list)
type sparse_row = Motifs.Motif.t * (image list)
type t = sparse_row list

val to_json : t -> Yojson.Basic.t
val of_motifs : Motifs.Motif.t list -> t
val add_results : filename -> (Core.Identifier.t list) list -> t -> t