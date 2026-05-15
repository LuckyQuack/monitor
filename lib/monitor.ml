(** Core monitor loop for the KWK / IVIVIV Big Cartel monitor.

    Provides {!run_once}, which performs a single polling cycle: fetch the
    current product list, diff it against the persisted state, update the
    state ref, and return the list of detected {!Types.change} values.

    {!log_change} prints a human-readable summary of a single change to
    [stdout] via [Printf.printf]. *)

open Types

(* ---------------------------------------------------------------------------
   Logging helper
   --------------------------------------------------------------------------- *)

(** [log_change change] prints a one-line human-readable summary of [change]
    to [stdout]. *)
let log_change (change : change) : unit =
  match change with
  | New p ->
    Printf.printf "[NEW]          %s — %s — %s\n%!"
      p.name
      (Utils.format_price p.price)
      p.product_url
  | Restocked p ->
    Printf.printf "[RESTOCKED]    %s — %s — %s\n%!"
      p.name
      (Utils.format_price p.price)
      p.product_url
  | PriceChanged { product = p; old_price } ->
    Printf.printf "[PRICE CHANGE] %s — %s -> %s — %s\n%!"
      p.name
      (Utils.format_price old_price)
      (Utils.format_price p.price)
      p.product_url
  | Updated { old_product; new_product } ->
    Printf.printf "[UPDATED]      %s (was: %s) — %s\n%!"
      new_product.name
      old_product.name
      new_product.product_url

(* ---------------------------------------------------------------------------
   run_once
   --------------------------------------------------------------------------- *)

(** [run_once ~sw ~net ~config ~state] performs one monitoring cycle.

    Steps:
    {ol
    {- Fetch the current product list via {!Http_client.fetch_products}.}
    {- Parse the JSON body with {!Http_client.parse_products}.}
    {- Diff new products against the persisted [state] to detect
       {!Types.New}, {!Types.Restocked}, and {!Types.PriceChanged} events.}
    {- Deduplicate: products detected as both [New] and [Restocked] are
       kept only as [Restocked] (they were sold-out before, not truly new).}
    {- Update [state] in place and persist it to disk.}
    {- Return [Ok changes] on success, or [Error msg] on any failure.}}
*)
let run_once
    ~(sw     : Eio.Switch.t)
    ~env
    ~(config : Config.t)
    ~(state  : monitor_state ref)
    ~(jar    : Cookie_jar.t)
  : (change list, string) result =

  (* Step 1 — fetch *)
  let fetch_result = Http_client.fetch_products ~sw ~env ~jar () in
  match fetch_result with
  | Http_client.Network_error msg ->
    Error ("fetch failed: " ^ msg)
  | Http_client.Http_error code ->
    Error (Printf.sprintf "HTTP %d" code)
  | Http_client.Success body ->

    (* Step 2 — parse *)
    (match Http_client.parse_products body with
     | Error msg ->
       Error ("parse failed: " ^ msg)
     | Ok new_products ->

       (* Step 3 — diff *)
       let old_state  = !state in
       let old_map    = State.to_map old_state in

       (* Products that are entirely new to the store (never seen before). *)
       let new_changes =
         Models.find_new_products old_state.products new_products
         |> List.map (fun p -> New p)
       in

       (* Products that were sold-out and are now active again. *)
       let restock_changes =
         Models.find_restocked old_state.products new_products
         |> List.map (fun p -> Restocked p)
       in

       (* Price (and field) changes for products we already know about. *)
       let field_changes =
         List.concat_map
           (fun (id, new_p) ->
              match Hashtbl.find_opt old_map id with
              | None       -> []          (* brand-new product, handled above *)
              | Some old_p -> Models.compare_products old_p new_p)
           new_products
       in

       (* Step 4 — deduplicate: if a product id appears in both new_changes
          and restock_changes, keep only the Restocked variant.

          A product is "restocked" when it was previously sold-out (i.e. it
          existed in the old state).  find_new_products only returns ids that
          are absent from old_state.products, so the two sets are already
          disjoint by construction — but we guard against future changes to
          find_new_products by filtering explicitly. *)
       let restock_ids =
         List.filter_map
           (function Restocked p -> Some p.id | _ -> None)
           restock_changes
       in
       let deduped_new_changes =
         List.filter
           (function
             | New p -> not (List.mem p.id restock_ids)
             | _     -> true)
           new_changes
       in

       let changes =
         deduped_new_changes @ restock_changes @ field_changes
       in

       (* Step 5 — update state *)
       state := State.update !state new_products (Utils.now_iso8601 ());
       State.save config.state_file !state;

       Ok changes)
