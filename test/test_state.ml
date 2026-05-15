(** Alcotest suite for [Kwk_monitor_lib.State]. *)

open Kwk_monitor_lib.Types
open Kwk_monitor_lib.State

(* ---------------------------------------------------------------------------
   Fixture helpers
   --------------------------------------------------------------------------- *)

let make_product
    ?(id = 1)
    ?(name = "Test Product")
    ?(permalink = "test-product")
    ?(price = 29.99)
    ?(on_sale = false)
    ?(status = "active")
    ?(created_at = "2024-01-01T00:00:00Z")
    ?(image_url = None)
    () =
  {
    id;
    name;
    permalink;
    price;
    on_sale;
    status;
    created_at;
    image_url;
    product_url = "https://iviviv.bigcartel.com/" ^ permalink;
  }

(* ---------------------------------------------------------------------------
   Tests
   --------------------------------------------------------------------------- *)

let test_empty () =
  let s = empty () in
  Alcotest.(check int)    "empty products list has length 0" 0 (List.length s.products);
  Alcotest.(check string) "empty fetched_at is empty string" "" s.fetched_at

let test_to_map_lookup () =
  let p1 = make_product ~id:1 ~name:"Alpha" ~price:10.00 () in
  let p2 = make_product ~id:2 ~name:"Beta"  ~price:20.00 () in
  let state = { products = [ (1, p1); (2, p2) ]; fetched_at = "2024-01-01T00:00:00Z" } in
  let tbl = to_map state in
  Alcotest.(check int)    "table has 2 entries"              2       (Hashtbl.length tbl);
  Alcotest.(check string) "id 1 maps to Alpha"              "Alpha" (Hashtbl.find tbl 1).name;
  Alcotest.(check string) "id 2 maps to Beta"               "Beta"  (Hashtbl.find tbl 2).name

let test_update () =
  let old_p = make_product ~id:1 ~name:"Old"   () in
  let new_p = make_product ~id:2 ~name:"Fresh" () in
  let old_state = { products = [ (1, old_p) ]; fetched_at = "old-ts" } in
  let new_products = [ (2, new_p) ] in
  let ts = "2025-06-01T12:00:00Z" in
  let s = update old_state new_products ts in
  Alcotest.(check int)    "products list replaced"    1   (List.length s.products);
  Alcotest.(check string) "fetched_at updated"        ts  s.fetched_at;
  Alcotest.(check string) "new product name correct"  "Fresh"
    (snd (List.hd s.products)).name

let test_save_load_roundtrip () =
  let tmp = Filename.temp_file "kwk_state_test" ".json" in
  Fun.protect
    ~finally:(fun () -> (try Sys.remove tmp with Sys_error _ -> ()))
    (fun () ->
       let p = make_product ~id:42 ~name:"Drop Tee" ~price:55.00 () in
       let state = { products = [ (42, p) ]; fetched_at = "2025-01-01T00:00:00Z" } in
       save tmp state;
       let loaded = load tmp in
       Alcotest.(check int)
         "round-trip: one product"   1    (List.length loaded.products);
       let (id, lp) = List.hd loaded.products in
       Alcotest.(check int)    "round-trip: product id"    42        id;
       Alcotest.(check string) "round-trip: product name"  "Drop Tee" lp.name;
       Alcotest.(check (float 0.001)) "round-trip: product price" 55.00 lp.price)

let test_load_nonexistent () =
  let path = "/tmp/kwk_state_does_not_exist_xyzzy.json" in
  let s = load path in
  Alcotest.(check int)    "nonexistent file: empty products" 0  (List.length s.products);
  Alcotest.(check string) "nonexistent file: empty fetched_at" "" s.fetched_at

let test_load_malformed () =
  let tmp = Filename.temp_file "kwk_state_bad" ".json" in
  Fun.protect
    ~finally:(fun () -> (try Sys.remove tmp with Sys_error _ -> ()))
    (fun () ->
       (* Write invalid JSON content *)
       let oc = open_out tmp in
       output_string oc "not json";
       close_out oc;
       let s = load tmp in
       Alcotest.(check int)    "malformed: empty products"   0  (List.length s.products);
       Alcotest.(check string) "malformed: empty fetched_at" "" s.fetched_at)

(* ---------------------------------------------------------------------------
   Runner
   --------------------------------------------------------------------------- *)

let () =
  Alcotest.run "State"
    [ ( "state"
      , [ Alcotest.test_case "empty () returns empty state"                `Quick test_empty
        ; Alcotest.test_case "to_map: lookup by id returns correct product" `Quick test_to_map_lookup
        ; Alcotest.test_case "update: replaces products and fetched_at"    `Quick test_update
        ; Alcotest.test_case "save then load round-trip"                   `Quick test_save_load_roundtrip
        ; Alcotest.test_case "load non-existent path returns empty state"  `Quick test_load_nonexistent
        ; Alcotest.test_case "load malformed JSON returns empty state"     `Quick test_load_malformed
        ] )
    ]
