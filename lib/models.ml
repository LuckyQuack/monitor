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
      Updated { old_product = old_p; new_product = new_p } :: changes
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
  let old_ids = List.map fst old_list in
  List.filter_map
    (fun (id, p) ->
       if List.mem id old_ids then None else Some p)
    new_list

(** [find_restocked old_list new_list] returns products that were unavailable
    in [old_list] and are now available in [new_list]. *)
let find_restocked
    (old_list : (int * product) list)
    (new_list : (int * product) list)
    : product list =
  List.filter_map
    (fun (id, new_p) ->
       match List.assoc_opt id old_list with
       | Some old_p when not (is_available old_p) && is_available new_p ->
         Some new_p
       | _ -> None)
    new_list
