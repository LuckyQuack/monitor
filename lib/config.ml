(** Configuration record and environment-variable loader. *)

(* ---------------------------------------------------------------------------
   Type
   --------------------------------------------------------------------------- *)

type t = {
  discord_webhook_url : string;
  store_url           : string;
  poll_interval_sec   : int;
  log_level           : string;
  state_file          : string;
}

(* ---------------------------------------------------------------------------
   Defaults
   --------------------------------------------------------------------------- *)

let default_poll_interval_sec = 540
let default_log_level         = "info"
let default_state_file        = "./data/last_state.json"

(* ---------------------------------------------------------------------------
   Loader
   --------------------------------------------------------------------------- *)

(** [load ()] reads configuration from environment variables and returns a {!t}.

    Required env vars:
    - [DISCORD_WEBHOOK_URL] — raises [Failure] if absent.
    - [STORE_URL]           — raises [Failure] if absent. Trailing slash is stripped.

    Optional env vars (with defaults):
    - [POLL_INTERVAL_SEC]   (default: 540)
    - [LOG_LEVEL]           (default: "info")
    - [STATE_FILE]          (default: "./data/last_state.json") *)
let load () =
  (try ignore (Dotenv.export ()) with _ -> ());
  let discord_webhook_url =
    match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with
    | Some url when url <> "" -> url
    | _ -> failwith "DISCORD_WEBHOOK_URL env var is required"
  in
  let store_url =
    match Sys.getenv_opt "STORE_URL" with
    | Some s when s <> "" ->
      (* strip trailing slash for consistent URL construction downstream *)
      if String.length s > 0 && s.[String.length s - 1] = '/' then
        String.sub s 0 (String.length s - 1)
      else s
    | _ -> failwith "STORE_URL env var is required"
  in
  let poll_interval_sec =
    match Sys.getenv_opt "POLL_INTERVAL_SEC" with
    | Some s -> (
        match int_of_string_opt s with
        | Some n -> n
        | None   -> default_poll_interval_sec)
    | None -> default_poll_interval_sec
  in
  let log_level =
    match Sys.getenv_opt "LOG_LEVEL" with
    | Some s when s <> "" -> s
    | _                   -> default_log_level
  in
  let state_file =
    match Sys.getenv_opt "STATE_FILE" with
    | Some s when s <> "" -> s
    | _                   -> default_state_file
  in
  { discord_webhook_url; store_url; poll_interval_sec; log_level; state_file }
