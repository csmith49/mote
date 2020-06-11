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

let of_jsonl parser filename = 
    let objs = ref [] in
    let stream = Yojson.Basic.linestream_from_file filename in
    let _ = Stream.iter (fun line ->
        let obj = match line with
            | `Json json -> parser json
            | _ -> None
        in match obj with
            | Some obj -> objs := obj :: !objs
            | _ -> ()
    ) stream in
    !objs

let to_jsonl obj_to_json objs filename =
    let out_channel = open_out filename in
    let jsons = CCList.map obj_to_json objs in
    CCList.iter (fun json -> let _ = Yojson.Basic.to_channel out_channel json in output_string out_channel "\n") jsons 