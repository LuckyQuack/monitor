let run () =
  let config =
    try Kwk_monitor_lib.Config.load ()
    with Failure msg ->
      Printf.eprintf "Configuration error: %s\n" msg;
      exit 1
  in

  Logs.set_reporter (Logs_fmt.reporter ());
  let level = match config.log_level with
    | "debug" -> Logs.Debug
    | "info"  -> Logs.Info
    | _       -> Logs.Warning
  in
  Logs.set_level (Some level);
  (* Clamp third-party sources (Piaf, TLS, etc.) to Warning so connection
     noise doesn't drown out our own output *)
  List.iter (fun src ->
    if Logs.Src.level src = None then
      Logs.Src.set_level src (Some Logs.Warning)
  ) (Logs.Src.list ());

  let state = ref (Kwk_monitor_lib.State.load config.state_file) in
  let jar   = Kwk_monitor_lib.Cookie_jar.create () in

  Sys.set_signal Sys.sigint (Sys.Signal_handle (fun _ ->
    Printf.printf "\nShutting down...\n%!";
    exit 0));

  Printf.printf "IVIVIV Monitor starting. Poll interval: %ds\n%!" config.poll_interval_sec;

  Eio_main.run @@ fun env ->

  while true do
    (Eio.Switch.run @@ fun sw ->
      match Kwk_monitor_lib.Monitor.run_once ~sw ~env ~config ~state ~jar with
      | Ok [] ->
          Logs.info (fun m -> m "No changes detected")
      | Ok changes ->
          List.iter Kwk_monitor_lib.Monitor.log_change changes;
          Kwk_monitor_lib.Notifier.send_changes ~sw ~env
            ~webhook_url:config.discord_webhook_url changes
      | Error msg ->
          Logs.err (fun m -> m "Monitor error: %s" msg));
    Kwk_monitor_lib.Utils.jittered_sleep
      ~base_sec:config.poll_interval_sec ~jitter:60 ()
  done

let () = run ()
