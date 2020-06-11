type filename = string
type image = filename * (Core.Identifier.t list)
type sparse_row = Motifs.Motif.t * (image list)
type t = sparse_row list


let rec to_json img =
    `List (CCList.map row_to_json img)
and row_to_json (motif, images) =
    `Assoc [
        ("motif", Motifs.Motif.to_json motif);
        ("images", `List (CCList.map image_to_json images))
    ]
and image_to_json (filename, ids) =
    `Assoc [
        ("file", `String filename);
        ("image", `List (CCList.map Core.Identifier.to_json ids))
    ]

let of_motifs motifs = CCList.map (fun m -> (m, [])) motifs

let add_results filename images sparse = CCList.map2
    (fun img -> fun (m, imgs) ->
        (m, (filename, img) :: imgs)
    ) images sparse