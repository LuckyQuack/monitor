(** State persistence for the KWK / IVIVIV Big Cartel monitor.

    Handles loading and saving the {!Types.monitor_state} to disk as JSON,
    and provides helpers for working with the in-memory representation. *)

open Types

(* ---------------------------------------------------------------------------
   Constructors
   --------------------------------------------------------------------------- *)

(** [empty ()] returns an initial, empty monitor state. *)
let empty () : monitor_state =
  { products = []; fetched_at = "" }

(* ---------------------------------------------------------------------------
   Persistence
   --------------------------------------------------------------------------- *)

(** [load path] reads a {!monitor_state} from the JSON file at [path].

    - If the file does not exist, returns {!empty}.
    - If the file exists but cannot be parsed, logs a warning to [stderr] and
      returns {!empty}. *)
let load (path : string) : monitor_state =
  if not (Sys.file_exists path) then
    empty ()
  else
    match
      let json = Yojson.Safe.from_file path in
      monitor_state_of_yojson json
    with
    | Ok state -> state
    | Error msg ->
      Printf.eprintf
        "[state] Warning: could not parse state file %S: %s\n%!" path msg;
      empty ()
    | exception exn ->
      Printf.eprintf
        "[state] Warning: failed to read state file %S: %s\n%!"
        path (Printexc.to_string exn);
      empty ()

(** [ensure_dir path] creates the parent directory of [path] if it does not
    exist. Raises {!Sys_error} on permission failure. *)
let ensure_dir (path : string) : unit =
  let dir = Filename.dirname path in
  if dir <> "" && dir <> "." && not (Sys.file_exists dir) then
    Unix.mkdir dir 0o755

(** [save path state] serialises [state] to JSON and writes it to [path].

    Creates the parent directory if it does not exist.
    Returns [Error msg] if the file cannot be written. *)
let save (path : string) (state : monitor_state) : (unit, string) result =
  try
    ensure_dir path;
    Yojson.Safe.to_file path (monitor_state_to_yojson state);
    Ok ()
  with Sys_error msg -> Error msg

(* ---------------------------------------------------------------------------
   Helpers
   --------------------------------------------------------------------------- *)

(** [to_map state] builds a hash table keyed by product id for O(1) lookups. *)
let to_map (state : monitor_state) : (int, product) Hashtbl.t =
  let tbl = Hashtbl.create (List.length state.products) in
  List.iter (fun (id, p) -> Hashtbl.replace tbl id p) state.products;
  tbl

(** [update state new_products fetched_at] returns a new state whose product
    list is [new_products] and whose timestamp is [fetched_at]. *)
let update
    (_state : monitor_state)
    (new_products : (int * product) list)
    (fetched_at : string)
    : monitor_state =
  { products = new_products; fetched_at }
