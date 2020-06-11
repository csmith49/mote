(** Identifiers represent unique names for objects *)
type t
type identifier = t

(** JSON conversion *)
val of_json : Yojson.Basic.t -> t option
val to_json : t -> Yojson.Basic.t

val of_string : string -> t option
val to_string : t -> string

val of_int : int -> t

val compare : t -> t -> int
val equal : t -> t -> bool
val hash : t -> int

val default : t

val simplify : t list -> t -> t
