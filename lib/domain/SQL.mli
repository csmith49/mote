type db

exception SQLException of string

val of_string : string -> db
val close : db -> bool

val evaluate : db -> Motifs.Motif.t -> Core.Identifier.t list
