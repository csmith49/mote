type t = [
    | `Constant of string * Core.Value.t
]

module Lattice : sig
    val weaken : t -> t list
end

val compare : t -> t -> int
val equal : t -> t -> bool

val implies : t -> t -> bool
val (=>) : t -> t -> bool

val of_pair : (string * Core.Value.t) -> t

val apply : t -> Core.Value.Map.t -> bool

val to_json : t -> Yojson.Basic.t
val of_json : Yojson.Basic.t -> t option

val to_string : t -> string