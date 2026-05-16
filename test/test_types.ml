(** Alcotest suite for [Kwk_monitor_lib.Types] and [Kwk_monitor_lib.Models]. *)

open Kwk_monitor_lib.Types
open Kwk_monitor_lib.Models

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
    product_url = make_product_url ~store_url:"https://test.bigcartel.com" permalink;
  }

(* ---------------------------------------------------------------------------
   Types suite
   --------------------------------------------------------------------------- *)

let test_make_product_url () =
  let url = make_product_url ~store_url:"https://test.bigcartel.com" "my-cool-tee" in
  Alcotest.(check string)
    "correct URL"
    "https://test.bigcartel.com/product/my-cool-tee"
    url

let test_make_product_url_empty_permalink () =
  let url = make_product_url ~store_url:"https://test.bigcartel.com" "" in
  Alcotest.(check string)
    "empty permalink gives base URL"
    "https://test.bigcartel.com/product/"
    url

let test_is_available_active () =
  let p = make_product ~status:"active" () in
  Alcotest.(check bool) "active product is available" true (is_available p)

let test_is_available_sold_out () =
  let p = make_product ~status:"sold-out" () in
  Alcotest.(check bool) "sold_out product is not available" false (is_available p)

let test_is_available_unknown_status () =
  let p = make_product ~status:"pending" () in
  Alcotest.(check bool) "unknown status is not available" false (is_available p)

(* ---------------------------------------------------------------------------
   Models suite
   --------------------------------------------------------------------------- *)

let test_compare_products_no_change () =
  let p = make_product ~price:29.99 () in
  let changes = compare_products p p in
  Alcotest.(check int) "no changes when products identical" 0 (List.length changes)

let test_compare_products_price_changed () =
  let old_p = make_product ~price:29.99 () in
  let new_p = make_product ~price:19.99 () in
  let changes = compare_products old_p new_p in
  Alcotest.(check int) "one change detected" 1 (List.length changes);
  match changes with
  | [ PriceChanged { old_price; product } ] ->
    Alcotest.(check (float 0.001)) "old price preserved" 29.99 old_price;
    Alcotest.(check (float 0.001)) "new price on product" 19.99 product.price
  | _ ->
    Alcotest.fail "expected exactly one PriceChanged"

let test_compare_products_price_unchanged_epsilon () =
  let old_p = make_product ~price:29.99 () in
  let new_p = make_product ~price:(29.99 +. 0.0001) () in
  let changes = compare_products old_p new_p in
  Alcotest.(check int) "epsilon difference produces no change" 0 (List.length changes)

let test_compare_products_on_sale_flip () =
  let old_p = make_product ~on_sale:false () in
  let new_p = make_product ~on_sale:true  () in
  let changes = compare_products old_p new_p in
  Alcotest.(check int) "one change detected" 1 (List.length changes);
  match changes with
  | [ Updated { new_product; _ } ] ->
    Alcotest.(check bool) "new product is on sale" true new_product.on_sale
  | _ ->
    Alcotest.fail "expected exactly one Updated"

let test_compare_products_on_sale_unchanged () =
  let old_p = make_product ~on_sale:true () in
  let new_p = make_product ~on_sale:true () in
  let changes = compare_products old_p new_p in
  Alcotest.(check int) "no change when on_sale unchanged" 0 (List.length changes)

let test_find_new_products_all_new () =
  let old_list = [] in
  let new_list =
    [ (1, make_product ~id:1 ~name:"Alpha" ())
    ; (2, make_product ~id:2 ~name:"Beta"  ())
    ]
  in
  let result = find_new_products old_list new_list in
  Alcotest.(check int) "two new products" 2 (List.length result)

let test_find_new_products_none_new () =
  let p1 = make_product ~id:1 () in
  let p2 = make_product ~id:2 () in
  let old_list = [ (1, p1); (2, p2) ] in
  let new_list = [ (1, p1); (2, p2) ] in
  let result = find_new_products old_list new_list in
  Alcotest.(check int) "no new products" 0 (List.length result)

let test_find_new_products_partial () =
  let old_p = make_product ~id:1 ~name:"Existing" () in
  let new_p = make_product ~id:2 ~name:"Fresh Drop" () in
  let old_list = [ (1, old_p) ] in
  let new_list = [ (1, old_p); (2, new_p) ] in
  let result = find_new_products old_list new_list in
  Alcotest.(check int) "one new product" 1 (List.length result);
  Alcotest.(check string) "correct new product name" "Fresh Drop" (List.hd result).name

let test_find_restocked_basic () =
  let old_p = make_product ~id:1 ~status:"sold-out" () in
  let new_p = make_product ~id:1 ~status:"active"   () in
  let old_list = [ (1, old_p) ] in
  let new_list = [ (1, new_p) ] in
  let result = find_restocked old_list new_list in
  Alcotest.(check int) "one restocked product" 1 (List.length result);
  Alcotest.(check string) "restocked product status is active" "active"
    (List.hd result).status

let test_find_restocked_was_active () =
  (* A product that was already active should not appear as restocked. *)
  let old_p = make_product ~id:1 ~status:"active" () in
  let new_p = make_product ~id:1 ~status:"active" () in
  let old_list = [ (1, old_p) ] in
  let new_list = [ (1, new_p) ] in
  let result = find_restocked old_list new_list in
  Alcotest.(check int) "no restock for already-active product" 0 (List.length result)

let test_find_restocked_still_sold_out () =
  let old_p = make_product ~id:1 ~status:"sold-out" () in
  let new_p = make_product ~id:1 ~status:"sold-out" () in
  let old_list = [ (1, old_p) ] in
  let new_list = [ (1, new_p) ] in
  let result = find_restocked old_list new_list in
  Alcotest.(check int) "still sold out — no restock" 0 (List.length result)

let test_find_restocked_new_product_not_counted () =
  (* A brand-new product (absent from old_list) should not be counted as restocked. *)
  let new_p = make_product ~id:99 ~status:"active" () in
  let old_list = [] in
  let new_list = [ (99, new_p) ] in
  let result = find_restocked old_list new_list in
  Alcotest.(check int) "new product is not a restock" 0 (List.length result)

(* ---------------------------------------------------------------------------
   Runner
   --------------------------------------------------------------------------- *)

let () =
  Alcotest.run "Types"
    [ ( "types"
      , [ Alcotest.test_case "make_product_url produces correct URL"        `Quick test_make_product_url
        ; Alcotest.test_case "make_product_url with empty permalink"        `Quick test_make_product_url_empty_permalink
        ; Alcotest.test_case "is_available true for active"                 `Quick test_is_available_active
        ; Alcotest.test_case "is_available false for sold_out"              `Quick test_is_available_sold_out
        ; Alcotest.test_case "is_available false for unknown status"        `Quick test_is_available_unknown_status
        ] )
    ; ( "models"
      , [ Alcotest.test_case "compare_products: no change"                  `Quick test_compare_products_no_change
        ; Alcotest.test_case "compare_products: price changed"              `Quick test_compare_products_price_changed
        ; Alcotest.test_case "compare_products: epsilon price unchanged"    `Quick test_compare_products_price_unchanged_epsilon
        ; Alcotest.test_case "compare_products: on_sale flip → Updated"   `Quick test_compare_products_on_sale_flip
        ; Alcotest.test_case "compare_products: on_sale unchanged"        `Quick test_compare_products_on_sale_unchanged
        ; Alcotest.test_case "find_new_products: all new"                   `Quick test_find_new_products_all_new
        ; Alcotest.test_case "find_new_products: none new"                  `Quick test_find_new_products_none_new
        ; Alcotest.test_case "find_new_products: partial overlap"           `Quick test_find_new_products_partial
        ; Alcotest.test_case "find_restocked: basic restock"                `Quick test_find_restocked_basic
        ; Alcotest.test_case "find_restocked: was already active"           `Quick test_find_restocked_was_active
        ; Alcotest.test_case "find_restocked: still sold out"               `Quick test_find_restocked_still_sold_out
        ; Alcotest.test_case "find_restocked: new product not a restock"    `Quick test_find_restocked_new_product_not_counted
        ] )
    ]
