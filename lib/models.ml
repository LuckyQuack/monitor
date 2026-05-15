(** Utility functions that operate on the domain types defined in [Types]. *)

open Types

(* ---------------------------------------------------------------------------
   Product comparison
   --------------------------------------------------------------------------- *)

(** [compare_products old_p new_p] returns the list of [change] values that
    describe what has changed between the two snapshots of the same product.

    Currently detects:
    - [PriceChanged] when [old_p.price <> new_p.price]

    Returns [[]] when the products are identical in all tracked fields. *)
let compare_products (old_p : product) (new_p : product) : change list =
  let changes = [] in
  (* Price check — use a small epsilon to handle float rounding. *)
  let changes =
    if Float.abs (old_p.price -. new_p.price) > 0.001 then
      PriceChanged { product = new_p; old_price = old_p.price } :: changes
    else
      changes
  in
  changes

(* ---------------------------------------------------------------------------
   Set-difference helpers
   --------------------------------------------------------------------------- *)

(** [find_new_products old_list new_list] returns the products that appear in
    [new_list] (by [id]) but are absent from [old_list].  These represent
    products that have been added to the store since the last snapshot. *)
let find_new_products
    (old_list : (int * product) list)
    (new_list : (int * product) list)
    : product list =
  let old_ids = List.map fst old_list in
  List.filter_map
    (fun (id, p) ->
       if List.mem id old_ids then None else Some p)
    new_list

(** [find_restocked old_list new_list] returns the products that were
    ["sold_out"] in [old_list] and are now ["active"] in [new_list].
    Products absent from either list are ignored. *)
let find_restocked
    (old_list : (int * product) list)
    (new_list : (int * product) list)
    : product list =
  List.filter_map
    (fun (id, new_p) ->
       match List.assoc_opt id old_list with
       | Some old_p
         when old_p.status = "sold-out" && new_p.status = "active" ->
         Some new_p
       | _ -> None)
    new_list
