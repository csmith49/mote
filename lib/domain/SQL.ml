type db = Sqlite3.db

exception SQLException of string
let handle_rc rc = match rc with
    | Sqlite3.Rc.OK -> ()
    | _ as e -> raise (SQLException (Sqlite3.Rc.to_string e))

let run db cb q =
    let rc = Sqlite3.exec_not_null_no_headers db ~cb:cb q in
        handle_rc rc

let of_string filename = Sqlite3.db_open filename
let close db = Sqlite3.db_close db

let id_to_column id = Printf.sprintf "_%s" (Core.Identifier.to_string id)

(* convert predicates to subqeuries *)
let predicate_to_subquery = function
    | `Constant (attr, value) -> Printf.sprintf
        "SELECT id FROM Attribute WHERE kind = %s AND value = %s"
        attr
        (Core.Value.to_string value)

(* and filters to combinations of queries *)
let rec filter_to_where_clause = function
    | [] -> "TRUE"
    | pred :: rest ->
        let rest = filter_to_where_clause rest in
        let pred = predicate_to_subquery pred in
        Printf.sprintf "id IN (%s) AND %s" pred rest

(* convert vertex to select statement *)
let vertex_to_select_statement id filter = Printf.sprintf
    "SELECT id AS %s FROM Vertex WHERE %s"
        (id_to_column id) 
        (filter_to_where_clause filter)

(* convert an edge to a select statement *)
let edge_to_select_statement (src, lbl, dest) = match lbl with
    | Motifs.Kinder.Star -> raise Not_found
    | Motifs.Kinder.Constant c -> Printf.sprintf
        "SELECT source AS %s, destination AS %s FROM Edge WHERE kind = %s"
            (id_to_column src)
            (id_to_column dest)
            (Core.Value.to_string c)

(* convert motif into sql statement *)
let motif_to_query motif =
    let structure = motif.Motifs.Motif.structure in
    let vertex_selects = structure
        |> Core.Structure.vertices
        |> CCList.filter_map (fun id -> match Core.Structure.label id structure with
            | Some lbl -> Some (id, lbl)
            | None -> None)
        |> CCList.map (fun (id, filt) -> vertex_to_select_statement id filt) in
    let edge_selects = structure
        |> Core.Structure.edges
        |> CCList.map edge_to_select_statement in
    let selects = (vertex_selects @ edge_selects)
        |> CCList.map (fun s -> "(" ^ s ^ ")")
        |> CCString.concat " NATURAL JOIN " in
    let query = Printf.sprintf "SELECT DISTINCT %s FROM %s"
        (id_to_column motif.Motifs.Motif.selector)
        (selects) in
    query

(* evaluating matchers *)
let evaluate db motif =
    let query = motif_to_query motif in
    let results = ref [] in
    let callback row = match CCArray.get row 0 |> Core.Identifier.of_string with
        | Some id -> results := id :: !results
        | None -> () in
    let _ = run db callback query in
        !results
