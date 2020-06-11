type t = Yojson.Basic.t

type 'a parser = t -> 'a option

val get : string -> 'a parser -> t -> 'a option
val assoc : 'a parser -> t -> (string * 'a) list option
val list : 'a parser -> t -> 'a list option
val one_or_more : 'a parser -> t -> 'a list option
val string : t -> string option
val int : t -> int option
val identity : t -> t option

(* reading and writing from files *)
val of_jsonl : 'a parser -> string -> 'a list
val to_jsonl : ('a -> Yojson.Basic.t) -> 'a list -> string -> unit