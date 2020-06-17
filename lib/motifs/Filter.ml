type t = Predicate.t list

let compare left right =
    let left' = left |> CCList.sort Predicate.compare in
    let right' = right |> CCList.sort Predicate.compare in
    CCList.compare Predicate.compare left' right'
let equal left right =
    let left' = left |> CCList.sort Predicate.compare in
    let right' = right |> CCList.sort Predicate.compare in
    CCList.equal Predicate.equal left' right'

let rec implies left right = match right with
    | [] -> true
    | pred :: rest ->
        (CCList.exists (fun l -> Predicate.(l => pred)) left) && (implies left rest)
let (=>) left right = implies left right

module Lattice = struct
    let rec weaken = function
        | [] -> [ [] ]
        | x :: rest -> [x] :: (weaken rest)
    
    let rec power_weaken = function
        | [] -> [ [] ]
        | x :: rest -> 
            let weakened = power_weaken rest in
            weakened @ (CCList.map (fun l -> x :: l) weakened)

    (* TODO - this is a pretty weak form of join, as it avoids any Pred lattice structure *)
    let rec join preds = match preds with
        | [] -> []
        | pred :: [] -> pred
        | pred :: rest -> CCList.inter ~eq:Predicate.equal pred (join rest)

end

let of_map map = map
    |> Core.Value.Map.to_list
    |> CCList.map Predicate.of_pair

let empty = []

let to_json filter = `Assoc (filter
    |> CCList.map Predicate.to_json
    |> CCList.map (fun json -> match json with
        | `Assoc xs -> xs
        | _ -> [])
    |> CCList.flatten)

let of_json json = Utility.JSON.assoc Core.Value.of_json json
    |> CCOpt.map (CCList.map Predicate.of_pair)

let apply filter map = filter
    |> CCList.for_all (fun p -> Predicate.apply p map)

let rec to_string = function
    | [] -> "⊤"
    | p :: [] -> Predicate.to_string p
    | p :: rest -> Printf.sprintf
        "%s ∧ %s"
        (Predicate.to_string p)
        (to_string rest)