open Types

(** [compare_products old_p new_p] returns the list of [change] values that
    describe what has changed between the two snapshots of the same product.

    Detects:
    - [PriceChanged] when price differs by more than 0.001
    - [Updated]      when [on_sale] status flips

    Returns [[]] when the products are identical in all tracked fields. *)
let compare_products (old_p : product) (new_p : product) : change list =
  let changes = [] in
  let changes =
    if Float.abs (old_p.price -. new_p.price) > 0.001 then
      PriceChanged { product = new_p; old_price = old_p.price } :: changes
    else
      changes
  in
  let changes =
    if old_p.on_sale <> new_p.on_sale then
      SaleStatusChanged { product = new_p; sale_started = new_p.on_sale } :: changes
    else
      changes
  in
  changes

(** [find_new_products old_list new_list] returns products that appear in
    [new_list] (by [id]) but are absent from [old_list]. *)
let find_new_products
    (old_list : (int * product) list)
    (new_list : (int * product) list)
    : product list =
  let old_tbl = Hashtbl.create (List.length old_list) in
  List.iter (fun (id, _) -> Hashtbl.replace old_tbl id ()) old_list;
  List.filter_map
    (fun (id, p) -> if Hashtbl.mem old_tbl id then None else Some p)
    new_list

(** [find_restocked old_list new_list] returns products that were unavailable
    in [old_list] and are now available in [new_list]. *)
let find_restocked
    (old_list : (int * product) list)
    (new_list : (int * product) list)
    : product list =
  let old_tbl = Hashtbl.create (List.length old_list) in
  List.iter (fun (id, p) -> Hashtbl.replace old_tbl id p) old_list;
  List.filter_map
    (fun (id, new_p) ->
       match Hashtbl.find_opt old_tbl id with
       | Some old_p when not (is_available old_p) && is_available new_p ->
         Some new_p
       | _ -> None)
    new_list
