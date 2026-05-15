Technical Documentation: IVIVIV Big Cartel Sale Monitor (OCaml Edition)Version: 1.0
Language: OCaml 5.2+
Date: May 2025
Goal: Highly reliable, type-safe, and stealthy monitor for https://iviviv.bigcartel.com/ with Discord rich alerts.1. Why OCaml for This ProjectStrong static typing + exhaustive pattern matching → extremely low chance of runtime errors.
Native compilation → single static binary, minimal resource usage, consistent timing (good for stealth).
Excellent error handling culture (Result, Option).
Predictable performance and memory behavior (important for long-running daemons).

2. Architecture OverviewPrimary Data Source: https://iviviv.bigcartel.com/products.json (clean public JSON)
Fallback: HTML scraping with Lambdasoup if JSON changes
State Management: Local JSON file + in-memory Map
Concurrency: Eio (modern effect-based, preferred in 2025–2026)
HTTP Client: Piaf (fast, HTTP/2 support) or Cohttp + Eio
Notifications: Discord Webhook via HTTP POST with rich embeds
Configuration: .env + Cmdliner
Logging: Logs + structured output

3. Undetectability & Production PracticesRandomized User-Agent rotation (pool of 10+ real browsers)
Jittered polling interval (base 8–15 minutes + random variance)
Realistic HTTP headers + connection reuse
Exponential backoff on failures
Single request per cycle
Static binary (no dynamic library bloat)
Optional future proxy support (via piaf middleware)

4. Project Structure (Dune)bash

iviviv-monitor-ocaml/
├── dune-project
├── dune
├── bin/
│   └── main.ml
├── lib/
│   ├── config.ml
│   ├── models.ml
│   ├── http_client.ml
│   ├── monitor.ml
│   ├── notifier.ml
│   ├── state.ml
│   ├── utils.ml
│   └── types.ml
├── data/
│   └── last_state.json          # gitignored
├── .env.example
├── README.md
└── opam                        # or use dune pin

5. OPAM / Dune Dependencies (dune-project)scheme

(executable
 (name main)
 (libraries 
   eio eio_main 
   piaf yojson ppx_deriving 
   logs fmt logs.fmt 
   dotenv cmdliner))

; Recommended packages:
; - eio + eio_main
; - piaf (or cohttp-eio)
; - yojson + ppx_deriving_yojson
; - logs + fmt
; - dotenv or conf-libyaml
; - base, core (optional for Map/Set helpers)

6. Core Modules Designtypes.ml / models.ml (Strongly Typed)ocaml

type product = {
  id : int;
  name : string;
  permalink : string;
  price : float;
  on_sale : bool;
  status : string;        (* "active", "sold_out", etc. *)
  created_at : string;
  image_url : string option;
  product_url : string;
} [@@deriving yojson, show]

type change =
  | New of product
  | Updated of product * product  (* old, new *)

http_client.mlCentralized HTTP client with rotating headers
Retry logic with exponential backoff
Proper connection pooling via Eio

monitor.ml (Core Logic)Fetch products
Compare with previous state (Map.Int.t)
Detect new items, price changes, sale status, restocks
Trigger notifications

notifier.mlRich Discord embeds:Product name + direct link
Current price / original price
Sale badge (green/red)
Thumbnail image
Timestamp + "New Drop" / "Restock" label

utils.mlrandom_user_agent ()
jittered_sleep ~base_sec:480 ~jitter:90 ()
SHA256 hashing for fallback detection

7. Main Loop (main.ml)ocaml

let run () =
  Eio_main.run @@ fun env ->
  let config = Config.load () in
  let state = ref (State.load ()) in
  
  while true do
    match Monitor.run_once env state with
    | Ok changes ->
        List.iter (Notifier.send config.discord_webhook) changes;
        State.save !state
    | Error e ->
        Logs.err (fun m -> m "Monitor error: %a" Error.pp e);
        Utils.sleep_with_backoff ();
    
    Utils.jittered_sleep ~base_sec:config.poll_interval ();
  done

8. Build & Deploymentbash

# Build
dune build --profile release

# Static binary (musl)
dune build --profile release --static

# Run
./_build/default/bin/main.exe

Deployment options:Minimal VPS (Hetzner, DigitalOcean, etc.)
Systemd service
Docker (very small image possible)
Single binary copy-paste deployment

9. Security & OperationsSecrets only in .env (never committed)
Run as dedicated low-privilege user
Rotating logs
Health ping option (daily “Bot alive” message)
Graceful shutdown handling

10. Roadmap / Future ImprovementsHTML fallback scraper with Lambdasoup
Proxy rotation support
Browser automation fallback (via playwright-ocaml or selenium binding) for very stealthy checks
ATDgen for even stronger JSON typing
Prometheus metrics endpoint (optional)
