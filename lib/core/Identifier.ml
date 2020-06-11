(* identifiers are integers, nothing more *)
type t = int
type identifier = t

(* utilities for converting to and from datatypes *)
let of_json = Utility.JSON.int
let to_json id = `Int id

let of_string = int_of_string_opt
let of_int x = x

let to_string = string_of_int

(* because we're just using integers, we lift comparisons and hashes appropriately *)
let compare = CCInt.compare
let hash = CCInt.hash
let equal = CCInt.equal

(* and provide a default for the graph implementations *)
let default = 0

(* for simplifying sets of identifiers *)
let simplify ids id =
    let clean_ids = CCList.sort_uniq ~cmp:compare ids in
    match CCList.find_idx (fun cid -> id = cid) clean_ids with
        | Some (idx, _) -> idx
        | _ -> raise Not_found