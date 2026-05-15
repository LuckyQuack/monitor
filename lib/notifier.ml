(** Discord webhook notifier — sends rich embed messages for product changes. *)

(* ---------------------------------------------------------------------------
   Embed builder
   --------------------------------------------------------------------------- *)

(** [make_embed change] constructs a Discord embed JSON object for [change].

    Colours:
    - New          0x57F287  green
    - Restocked    0x5865F2  blurple
    - PriceChanged 0xFEE75C  yellow
    - Updated      0xEB459E  pink *)
let make_embed (change : Types.change) : Yojson.Safe.t =
  (* Extract the primary product and build variant-specific fields. *)
  let product, color, title, description =
    match change with
    | Types.New p ->
      let desc =
        Printf.sprintf "**Price:** %s\n[View Product](%s)"
          (Utils.format_price p.price)
          p.product_url
      in
      (p, 0x57F287, Printf.sprintf "\xf0\x9f\x86\x95 New Drop: %s" p.name, desc)

    | Types.Restocked p ->
      let desc =
        Printf.sprintf "**Price:** %s\n[View Product](%s)"
          (Utils.format_price p.price)
          p.product_url
      in
      (p, 0x5865F2, Printf.sprintf "\xf0\x9f\x94\x84 Restock: %s" p.name, desc)

    | Types.PriceChanged { product = p; old_price } ->
      let desc =
        Printf.sprintf "~~%s~~ \xe2\x86\x92 **%s**\n[View Product](%s)"
          (Utils.format_price old_price)
          (Utils.format_price p.price)
          p.product_url
      in
      (p, 0xFEE75C, Printf.sprintf "\xf0\x9f\x92\xb0 Price Drop: %s" p.name, desc)

    | Types.Updated { old_product = _; new_product } ->
      let desc =
        Printf.sprintf "**Price:** %s\n[View Product](%s)"
          (Utils.format_price new_product.price)
          new_product.product_url
      in
      ( new_product,
        0xEB459E,
        Printf.sprintf "\xe2\x9c\x8f\xef\xb8\x8f Updated: %s" new_product.name,
        desc )
  in
  (* Truncate long strings to stay within Discord embed limits. *)
  let title       = Utils.truncate_str ~max_len:256 title in
  let description = Utils.truncate_str ~max_len:4096 description in
  (* Base fields present on every embed. *)
  let base_fields : (string * Yojson.Safe.t) list =
    [ ("color",       `Int color)
    ; ("title",       `String title)
    ; ("description", `String description)
    ; ("url",         `String product.product_url)
    ; ("timestamp",   `String (Utils.now_iso8601 ()))
    ; ("footer",      `Assoc [("text", `String "IVIVIV Monitor")])
    ]
  in
  (* Optionally attach a thumbnail when the product has an image. *)
  let fields =
    match product.image_url with
    | Some url ->
      base_fields @ [("thumbnail", `Assoc [("url", `String url)])]
    | None ->
      base_fields
  in
  `Assoc fields

(* ---------------------------------------------------------------------------
   HTTP send helpers
   --------------------------------------------------------------------------- *)

(** [send_change ~sw ~net ~webhook_url change] POSTs a single Discord embed for
    [change] to [webhook_url].

    Returns [Ok ()] when Discord responds with HTTP 204 (No Content), which is
    the documented success status for webhook executions.  Returns
    [Error msg] on any HTTP error or network failure. *)
let send_change
    ~(sw : Eio.Switch.t)
    ~env
    ~(webhook_url : string)
    (change : Types.change)
  : (unit, string) result =
  let embed    = make_embed change in
  let payload  = `Assoc [("embeds", `List [embed])] in
  let json_str = Yojson.Safe.to_string payload in
  let uri      = Uri.of_string webhook_url in
  let body     = Piaf.Body.of_string json_str in
  let headers  =
    [ ("content-type", "application/json")
    ; ("user-agent",   Utils.random_user_agent ())
    ]
  in
  match
    Piaf.Client.Oneshot.post ~sw ~headers ~body ~config:Piaf.Config.default
      env uri
  with
  | Ok response ->
    let code = Piaf.Status.to_code response.status in
    (* Drain the response body to avoid resource leaks. *)
    let _ = Piaf.Body.drain response.body in
    if code = 204 then Ok ()
    else Error (Printf.sprintf "HTTP %d" code)
  | Error err ->
    Error (Piaf.Error.to_string err)

(** [send_changes ~sw ~net ~webhook_url changes] sends one Discord notification
    per entry in [changes].

    Errors for individual changes are logged to stderr but do not abort the
    remaining sends.  A 500 ms pause between requests is inserted to respect
    Discord's rate limits. *)
let send_changes
    ~(sw : Eio.Switch.t)
    ~env
    ~(webhook_url : string)
    (changes : Types.change list)
  : unit =
  List.iter (fun change ->
    (match send_change ~sw ~env ~webhook_url change with
     | Ok ()    -> ()
     | Error msg ->
       Printf.eprintf "[notifier] failed to send Discord notification: %s\n%!"
         msg);
    Unix.sleepf 0.5
  ) changes
