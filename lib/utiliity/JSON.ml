type t = Yojson.Basic.t

type 'a parser = t -> 'a option

let get key parser json = match json with
    | `Assoc xs -> xs
        |> CCList.assoc_opt ~eq:(=) key
        |> CCOpt.flat_map parser
    | _ -> None

let assoc parser json = match json with
    | `Assoc xs -> xs
        |> CCList.map (fun (k, v) -> match parser v with
            | Some v -> Some (k, v)
            | None -> None)
        |> CCList.all_some
    | _ -> None

let list parser json = match json with
    | `List xs -> xs
        |> CCList.map parser
        |> CCList.all_some
    | _ -> None

let one_or_more parser json = match parser json with
    | Some v -> Some [v]
    | None -> list parser json

let string json = match json with
    | `String s -> Some s
    | _ -> None

let int json = match json with
    | `Int i -> Some i
    | _ -> None

let identity json = Some json