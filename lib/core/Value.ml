type t = [
    | `Int of int
    | `String of string
    | `Bool of bool
    | `Null
]
type value = t


let of_string : string -> t = fun s ->
    match int_of_string_opt s with
        | Some i -> `Int i
        | None -> match bool_of_string_opt s with
            | Some b -> `Bool b
            | None -> `String s


(* for printing *)
let to_string : t -> string = function
    | `Int i -> string_of_int i
    | `String s -> "'" ^ s ^ "'"
    | `Bool b -> (string_of_bool b)
    | `Null -> "NULL"

(* picking out particular constructors *)
let is_null : t -> bool = function
    | `Null -> true
    | _ -> false

(* comparisons don't really mean much with such a heterogeneous type *)
let compare = compare
let equal = (=)

(* converting from json representations *)
let of_json  = function
    | `Int i -> Some (`Int i)
    | `String s -> Some (`String s)
    | `Bool b -> Some (`Bool b)
    | `Null -> Some (`Null)
    | _ -> None
let to_json = function
    | `Int i -> `Int i
    | `String s -> `String s
    | `Bool b -> `Bool b
    | `Null -> `Null

(* utilities for encoding filters *)
module Utility = struct

    let equality = equal

    let substring large small = match large, small with
        | `String l, `String s ->
            not (CCString.find_all_l ~sub:s l |> CCList.is_empty)
        | _ -> false
end

(* indexing values by strings *)
module Map = struct
    (* we're always going to index by strings here *)
    module StringMap = CCMap.Make(CCString)
    
    (* concretize the type more *)
    type t = value StringMap.t

    (* expose some simple functions *)
    let empty : t = StringMap.empty
    let get : CCString.t -> t -> value option = StringMap.get
    let add : CCString.t -> value -> t -> t = StringMap.add
    let to_list : t -> (CCString.t * value) list = StringMap.bindings
    let of_list = StringMap.of_list

    let is_empty : t -> bool = StringMap.is_empty

    let keys : t -> string list = fun m -> StringMap.to_list m |> CCList.map fst
    let values : t -> value list = fun m -> StringMap.to_list m |> CCList.map snd


    let to_json = fun m -> 
        let ls = m 
            |> StringMap.to_list
            |> CCList.map (fun (k, v) -> (k, to_json v)) in
        `Assoc ls
    (* the getter from json *)
    let of_json = function
        | `Assoc attrs ->
            let binding_of_json (k, v) = match of_json v with
                | Some v -> Some (k, v)
                | _ -> None
            in CCList.map binding_of_json attrs
                |> CCOpt.sequence_l
                |> CCOpt.map StringMap.of_list
        | _ -> None

    (* string conversion *)
    let to_string : t -> string = fun map ->
        let map = map |> to_list
            |> CCList.map (fun (k, v) -> k ^ " : " ^ (to_string v))
            |> CCString.concat ", "
        in "[" ^ map ^ "]"
end

