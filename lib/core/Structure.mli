(** graph with vertices as identifiers *)
include Utility.Graph.GRAPH with type vertex := Identifier.t

(** for access in submodules, we provide a type alias *)
type ('v, 'e) structure = ('v, 'e) t

(** {1 JSON} *)

(** given parsers for vertex and edge labels, generates a json parser *)
val of_json : 
    (Yojson.Basic.t -> 'v option) -> 'v ->
    (Yojson.Basic.t -> 'e option) -> 'e ->
        Yojson.Basic.t -> ('v, 'e) t option

(** given converters for vertex and edge labels, generates a json converter *)
val to_json :
    ('v -> Yojson.Basic.t) ->
    ('e -> Yojson.Basic.t) ->
        ('v, 'e) t -> Yojson.Basic.t

(** {1 Utility} *)

(** maintains utility functions for manipulating edges *)
module Edge : sig
    (** given edge label equality, lifts equality to edges *)
    val equal : ('e -> 'e -> bool) -> 'e edge -> 'e edge -> bool

    (** given edge label comparison, lifts comparison to edges *)
    val compare : ('e -> 'e -> int) -> 'e edge -> 'e edge -> int

    (** returns source of an edge *)
    val source : 'e edge -> Identifier.t

    (** returns destination of edge *)
    val destination : 'e edge -> Identifier.t

    (** returns label of edge *)
    val label : 'e edge -> 'e
end

(* * given string converters for vertices and edges, builds a string converter *)
val to_string : ('v -> string) -> ('e -> string) -> ('v, 'e) t -> string


module Embedding : sig
    (** embeddings are bijective functions from identifiers to identifiers *)
    type t

    (** returns all identifiers in the domain of the embedding *)
    val domain : t -> Identifier.t list

    (** returns all identifiers in the codomain of the embedding *)
    val codomain : t -> Identifier.t list

    (** constructs an empty embedding *)
    val empty : t

    (** extends an embedding with a left/right pair *)
    val extend : Identifier.t -> Identifier.t -> t -> t

    (** makes an embedding from a single left/right pair *)
    val embed : Identifier.t -> Identifier.t -> t

    (** looks up the image of an identifier in the embedding *)
    val image : Identifier.t -> t -> Identifier.t option

    (** looks up the preimage of an identifier in the embedding *)
    val preimage : Identifier.t -> t -> Identifier.t option

    (** converts an edge using image *)
    val edge_image : 'e edge -> t -> 'e edge option

    (** applies an embedding to a structure *)
    val embed_structure : ('v, 'e) structure -> t -> ('v, 'e) structure option
end

module BiPath : sig
    type 'e t

    val of_forward_edge : 'e edge -> 'e t
    val of_backward_edge : 'e edge -> 'e t

    val extend : 'e t -> 'e edge -> 'e t option
    
    val edges : 'e t -> 'e edge list

    val source : 'e t -> Identifier.t
    val destination : 'e t -> Identifier.t

    val loop_free : 'e t -> bool

    val between : ('v, 'e) structure -> Identifier.t -> Identifier.t -> 'e t list
end

module Algorithms : sig
    val neighborhood : ('v, 'e) structure -> Identifier.t list -> int -> Identifier.t list
    val reachable : ('v, 'e) structure -> Identifier.t list -> Identifier.t list
    val bireachable : ('v, 'e) structure -> Identifier.t list -> Identifier.t list
end