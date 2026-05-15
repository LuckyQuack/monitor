(** HTTP client for the KWK / IVIVIV Big Cartel monitor.

    Provides a single entry-point {!fetch_products} that fetches the store's
    products JSON over HTTPS using Piaf (within an Eio fiber), with rotating
    user-agents, realistic browser headers, and exponential-backoff retries.

    {!parse_products} converts the raw JSON body into a list of
    [(id * Types.product)] pairs suitable for {!State.update}. *)

(* ---------------------------------------------------------------------------
   Result type
   --------------------------------------------------------------------------- *)

(** The outcome of a single HTTP fetch attempt. *)
type fetch_result =
  | Success       of string   (** Raw response body on HTTP 200. *)
  | Http_error    of int      (** Non-200 HTTP status code. *)
  | Network_error of string   (** Low-level error or exception message. *)

(* ---------------------------------------------------------------------------
   Constants
   --------------------------------------------------------------------------- *)

let products_url = "https://iviviv.bigcartel.com/products.json"

(* ---------------------------------------------------------------------------
   Internal: single attempt
   --------------------------------------------------------------------------- *)

(** Perform exactly one GET request and return a {!fetch_result}. *)
let fetch_once ~sw ~env () =
  let uri = Uri.of_string products_url in
  let base_headers =
    [ ("user-agent",      Utils.random_user_agent ())
    ; ("accept",          "application/json")
    ; ("accept-language", "en-US,en;q=0.9")
    ; ("cache-control",   "no-cache")
    ; ("connection",      "keep-alive")
    ; ("referer",         "https://iviviv.bigcartel.com/")
    ]
  in
  let headers = match Cookie_jar.header () with
    | None        -> base_headers
    | Some cookie -> ("cookie", cookie) :: base_headers
  in
  match
    Piaf.Client.Oneshot.get ~sw ~headers ~config:Piaf.Config.default env uri
  with
  | Error err ->
    Network_error (Piaf.Error.to_string err)
  | Ok response ->
    (* Persist any cookies the server sets for the next request *)
    let set_cookies =
      Piaf.Headers.to_list response.headers
      |> List.filter_map (fun (name, value) ->
           if String.lowercase_ascii name = "set-cookie" then Some value
           else None)
    in
    Cookie_jar.update set_cookies;
    let status = Piaf.Status.to_code response.status in
    if status = 200 then
      match Piaf.Body.to_string response.body with
      | Ok body   -> Success body
      | Error err -> Network_error (Piaf.Error.to_string err)
    else
      Http_error status

(* ---------------------------------------------------------------------------
   Public: fetch_products
   --------------------------------------------------------------------------- *)

(** [fetch_products ~sw ~net ?max_retries ?backoff_base_sec ()] fetches the
    products JSON from Big Cartel.

    Retries up to [max_retries] additional times (default 3) on any non-Success
    result, sleeping for [backoff_base_sec * 2^attempt] seconds between
    attempts (default base 30 s). *)
let fetch_products
    ~sw
    ~env
    ?(max_retries    = 3)
    ?(backoff_base_sec = 30)
    ()
  =
  let rec loop attempt =
    let result = fetch_once ~sw ~env () in
    match result with
    | Success _ ->
      result
    | Http_error _ | Network_error _ when attempt < max_retries ->
      let delay = backoff_base_sec * (1 lsl attempt) in
      Unix.sleepf (float_of_int delay);
      loop (attempt + 1)
    | Http_error _ | Network_error _ ->
      result
  in
  loop 0

(* ---------------------------------------------------------------------------
   Public: parse_products
   --------------------------------------------------------------------------- *)

(** [parse_products body] deserialises the raw JSON [body] returned by the
    Big Cartel products endpoint into a list of [(id, product)] pairs.

    Returns [Error msg] on any parse failure. *)
let parse_products (body : string) : ((int * Types.product) list, string) result =
  try
    let open Yojson.Safe.Util in
    let json  = Yojson.Safe.from_string body in
    let items = to_list json in
    let pairs =
      List.map
        (fun item ->
           let id         = item |> member "id"         |> to_int in
           let name       = item |> member "name"       |> to_string in
           let permalink  = item |> member "permalink"  |> to_string in
           let price =
             (* price may be an int or float in the JSON *)
             match member "price" item with
             | `Int   n -> float_of_int n
             | `Float f -> f
             | other    -> to_float other
           in
           let on_sale    = item |> member "on_sale"    |> to_bool in
           let status     = item |> member "status"     |> to_string in
           let created_at = item |> member "created_at" |> to_string in
           (* images is optional; take the url of the first entry if present *)
           let image_url =
             match member "images" item with
             | `Null | `List [] -> None
             | `List (first :: _) ->
               (match member "url" first with
                | `String s -> Some s
                | _         -> None)
             | _ -> None
           in
           let product_url = Types.make_product_url permalink in
           let product : Types.product =
             { id
             ; name
             ; permalink
             ; price
             ; on_sale
             ; status
             ; created_at
             ; image_url
             ; product_url
             }
           in
           (id, product))
        items
    in
    Ok pairs
  with exn ->
    Error (Printexc.to_string exn)
