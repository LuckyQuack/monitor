(** Core domain types for the KWK / IVIVIV Big Cartel monitor. *)

(** A single product fetched from the Big Cartel store. *)
type product = {
  id         : int;
  name       : string;
  permalink  : string;
  price      : float;
  on_sale    : bool;
  status     : string;   (* "active" | "sold-out" *)
  created_at : string;
  image_url  : string option;
  product_url: string;
}
[@@deriving yojson, show]

(** A detected change between two snapshots of the store. *)
type change =
  | New               of product
  | Restocked         of product
  | PriceChanged      of { product : product; old_price : float }
  | SaleStatusChanged of { product : product; sale_started : bool }

(** The persisted state held between polling cycles. *)
type monitor_state = {
  products   : (int * product) list;
  fetched_at : string;
}
[@@deriving yojson, show]

(* ---------------------------------------------------------------------------
   Helpers
   --------------------------------------------------------------------------- *)

(** Build the canonical product URL for a Big Cartel permalink. *)
let make_product_url ~store_url permalink =
  store_url ^ "/product/" ^ permalink

(** [is_available p] is [true] when the product status is ["active"]. *)
let is_available p = p.status = "active"
