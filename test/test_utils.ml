(** Alcotest tests for Utils and Config modules. *)

open Kwk_monitor_lib.Utils
open Kwk_monitor_lib.Config

(* ---------------------------------------------------------------------------
   Utils tests
   --------------------------------------------------------------------------- *)

let test_random_user_agent_non_empty () =
  let ua = random_user_agent () in
  Alcotest.(check bool) "non-empty user-agent" true (String.length ua > 0)

let test_random_user_agent_in_pool () =
  (* Run 50 iterations; every result must be a member of user_agents. *)
  let pool = Array.to_list user_agents in
  for _ = 1 to 50 do
    let ua = random_user_agent () in
    let found = List.mem ua pool in
    Alcotest.(check bool) "user-agent is from pool" true found
  done

let test_format_price_cents () =
  Alcotest.(check string) "format 29.99" "$29.99" (format_price 29.99)

let test_format_price_round () =
  Alcotest.(check string) "format 100.00" "$100.00" (format_price 100.0)

let test_truncate_str_truncates () =
  Alcotest.(check string)
    "truncated with ellipsis"
    "Hello..."
    (truncate_str ~max_len:5 "Hello World")

let test_truncate_str_short () =
  Alcotest.(check string)
    "short string unchanged"
    "Short"
    (truncate_str ~max_len:20 "Short")

(** Minimal regex-like check: verify the ISO 8601 shape without a regex library.
    Pattern: YYYY-MM-DDTHH:MM:SSZ  (20 chars, fixed positions) *)
let test_now_iso8601_format () =
  let ts = now_iso8601 () in
  (* Length must be exactly 20. *)
  Alcotest.(check int) "iso8601 length" 20 (String.length ts);
  (* Fixed literal chars at known positions. *)
  Alcotest.(check char) "T separator"  'T' ts.[10];
  Alcotest.(check char) "Z suffix"     'Z' ts.[19];
  Alcotest.(check char) "date dash 1"  '-' ts.[4];
  Alcotest.(check char) "date dash 2"  '-' ts.[7];
  Alcotest.(check char) "time colon 1" ':' ts.[13];
  Alcotest.(check char) "time colon 2" ':' ts.[16];
  (* Every other character must be a digit. *)
  let digit_positions = [0;1;2;3; 5;6; 8;9; 11;12; 14;15; 17;18] in
  List.iter
    (fun i ->
       let c = ts.[i] in
       Alcotest.(check bool)
         (Printf.sprintf "digit at position %d" i)
         true
         (c >= '0' && c <= '9'))
    digit_positions

(* ---------------------------------------------------------------------------
   Config tests
   --------------------------------------------------------------------------- *)

let test_config_load_success () =
  Unix.putenv "DISCORD_WEBHOOK_URL" "https://discord.com/api/webhooks/test/token";
  Unix.putenv "POLL_INTERVAL_SEC"   "300";
  Unix.putenv "LOG_LEVEL"           "debug";
  Unix.putenv "STATE_FILE"          "/tmp/state.json";
  let cfg = load () in
  Alcotest.(check string) "webhook url"
    "https://discord.com/api/webhooks/test/token"
    cfg.discord_webhook_url;
  Alcotest.(check int)    "poll interval" 300     cfg.poll_interval_sec;
  Alcotest.(check string) "log level"     "debug" cfg.log_level;
  Alcotest.(check string) "state file"    "/tmp/state.json" cfg.state_file

let test_config_defaults () =
  Unix.putenv "DISCORD_WEBHOOK_URL" "https://discord.com/api/webhooks/x/y";
  (* Remove optional vars so defaults kick in. *)
  (try Unix.putenv "POLL_INTERVAL_SEC" "" with _ -> ());
  (try Unix.putenv "LOG_LEVEL"         "" with _ -> ());
  (try Unix.putenv "STATE_FILE"        "" with _ -> ());
  let cfg = load () in
  Alcotest.(check int)    "default poll interval" 540                     cfg.poll_interval_sec;
  Alcotest.(check string) "default log level"     "info"                  cfg.log_level;
  Alcotest.(check string) "default state file"    "./data/last_state.json" cfg.state_file

let test_config_missing_webhook () =
  (* Unset by setting to empty string; our loader treats that as missing. *)
  Unix.putenv "DISCORD_WEBHOOK_URL" "";
  let raised =
    try
      let _ = load () in false
    with Failure msg ->
      (* Verify the message is meaningful. *)
      String.length msg > 0
  in
  Alcotest.(check bool) "raises Failure when webhook missing" true raised

(* ---------------------------------------------------------------------------
   Entry point
   --------------------------------------------------------------------------- *)

let () =
  Alcotest.run "Utils"
    [ ( "utils",
        [ Alcotest.test_case "random_user_agent non-empty"  `Quick test_random_user_agent_non_empty
        ; Alcotest.test_case "random_user_agent in pool"    `Quick test_random_user_agent_in_pool
        ; Alcotest.test_case "format_price 29.99"           `Quick test_format_price_cents
        ; Alcotest.test_case "format_price 100.00"          `Quick test_format_price_round
        ; Alcotest.test_case "truncate_str truncates"       `Quick test_truncate_str_truncates
        ; Alcotest.test_case "truncate_str short unchanged" `Quick test_truncate_str_short
        ; Alcotest.test_case "now_iso8601 format"           `Quick test_now_iso8601_format
        ] )
    ; ( "config",
        [ Alcotest.test_case "load succeeds with env vars"      `Quick test_config_load_success
        ; Alcotest.test_case "load uses defaults"               `Quick test_config_defaults
        ; Alcotest.test_case "load raises on missing webhook"   `Quick test_config_missing_webhook
        ] )
    ]
