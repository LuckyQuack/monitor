(** Miscellaneous pure and side-effecting utility functions. *)

(* ---------------------------------------------------------------------------
   User-agent pool
   --------------------------------------------------------------------------- *)

(** Pool of real browser user-agent strings used to rotate request headers. *)
let user_agents = [|
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36";
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36";
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:125.0) Gecko/20100101 Firefox/125.0";
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 14_4_1) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4.1 Safari/605.1.15";
  "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36";
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36 Edg/123.0.0.0";
  "Mozilla/5.0 (iPhone; CPU iPhone OS 17_4_1 like Mac OS X) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.4.1 Mobile/15E148 Safari/604.1";
  "Mozilla/5.0 (Linux; Android 14; Pixel 8) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Mobile Safari/537.36";
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 OPR/110.0.0.0";
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/124.0.0.0 Safari/537.36 Vivaldi/6.7.3329.35";
|]

(** Returns a random user-agent string from the pool. *)
let random_user_agent () =
  user_agents.(Random.int (Array.length user_agents))

(* ---------------------------------------------------------------------------
   Timing helpers
   --------------------------------------------------------------------------- *)

(** [jittered_sleep ~clock ~base_sec ~jitter ()] sleeps for
    [max 1 (base_sec + rand_offset)] seconds using the Eio clock, where
    [rand_offset] is drawn uniformly from [-jitter, jitter). *)
let jittered_sleep ~clock ~base_sec ~jitter () =
  let offset = Random.int (jitter * 2) - jitter in
  let duration = max 1 (base_sec + offset) in
  Eio.Time.sleep clock (float_of_int duration)

(** Returns the current UTC time formatted as an ISO 8601 string
    of the form ["YYYY-MM-DDTHH:MM:SSZ"]. *)
let now_iso8601 () =
  let t  = Unix.gettimeofday () in
  let tm = Unix.gmtime t in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02dZ"
    (tm.Unix.tm_year + 1900)
    (tm.Unix.tm_mon  + 1)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

(* ---------------------------------------------------------------------------
   Formatting helpers
   --------------------------------------------------------------------------- *)

(** Formats a floating-point price as ["$XX.XX"]. *)
let format_price price =
  Printf.sprintf "$%.2f" price

(** [truncate_str ~max_len s] returns [s] unchanged when
    [String.length s <= max_len], otherwise returns the first [max_len]
    characters of [s] followed by ["..."]. *)
let truncate_str ~max_len s =
  if String.length s <= max_len then s
  else begin
    (* Walk back from max_len to find a valid UTF-8 start byte.
       Continuation bytes have the form 10xxxxxx (byte & 0xC0 = 0x80). *)
    let i = ref max_len in
    while !i > 0 && Char.code s.[!i] land 0xC0 = 0x80 do decr i done;
    String.sub s 0 !i ^ "..."
  end
