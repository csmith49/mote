type t =
    | Star
    | Constant of Core.Value.t

let of_value v = Constant v

let to_string = function
    | Star -> "*"
    | Constant v -> Core.Value.to_string v

let apply kinder value = match kinder with
    | Star -> true
    | Constant c -> Core.Value.equal c value

let compare left right = match left, right with
    | Star, Star -> 0
    | Star, _ -> -1
    | _, Star -> 1
    | Constant x, Constant y -> Core.Value.compare x y

let equal left right = match left, right with
    | Star, Star -> true
    | Constant x, Constant y when Core.Value.equal x y -> true
    | _ -> false

let implies left right = match left, right with
    | _, Star -> true
    | Constant x, Constant y -> Core.Value.equal x y
    | _ -> false
let (=>) left right = implies left right

let to_json = function
    | Star -> `Assoc [("kind", `String "*")]
    | Constant c -> `Assoc [
        ("kind", `String "constant");
        ("constant", Core.Value.to_json c)
    ]

let of_json json = match Core.Value.of_json json with
    | Some v -> Some (Constant v)
    | None -> None