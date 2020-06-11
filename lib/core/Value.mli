type t = [
    | `Int of int
    | `String of string
    | `Bool of bool
    | `Null
]

type value = t

val of_string : string -> t


val to_string : t -> string

val is_null : t -> bool

val compare : t -> t -> int
val equal : t -> t -> bool

val of_json : Yojson.Basic.t -> t option
val to_json : t -> Yojson.Basic.t

module Utility : sig
    val equality : t -> t -> bool
    val substring : t -> t -> bool
end

module Map : sig
    type t

    val empty : t
    val get : string -> t -> value option
    val add : string -> value -> t -> t
    
    val to_list : t -> (string * value) list
    val of_list : (string * value) list -> t
    
    val is_empty : t -> bool

    val keys : t -> string list
    val values : t -> value list

    val to_json : t -> Yojson.Basic.t
    val of_json : Yojson.Basic.t -> t option

    val to_string : t -> string
end