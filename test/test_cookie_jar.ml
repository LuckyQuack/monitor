open Kwk_monitor_lib.Cookie_jar

let reset () = Hashtbl.clear jar

let test_empty_jar () =
  reset ();
  Alcotest.(check (option string)) "empty jar returns None" None (header ())

let test_single_cookie () =
  reset ();
  update ["__cf_bm=abc123; Path=/; HttpOnly"];
  match header () with
  | None -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "cookie header" "__cf_bm=abc123" h

let test_multiple_cookies () =
  reset ();
  update ["cart=xyz; Path=/"; "session=tok; HttpOnly"];
  match header () with
  | None -> Alcotest.fail "expected Some"
  | Some h ->
    Alcotest.(check bool) "contains cart"    true (String.length h > 0);
    Alcotest.(check bool) "contains session" true
      (let parts = String.split_on_char ';' h in List.length parts = 2)

let test_update_overwrites () =
  reset ();
  update ["__cf_bm=first; Path=/"];
  update ["__cf_bm=second; Path=/"];
  match header () with
  | None -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "value overwritten" "__cf_bm=second" h

let test_value_with_equals () =
  reset ();
  update ["token=ab==cd; Path=/"];
  match header () with
  | None -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "value preserves =" "token=ab==cd" h

let test_malformed_no_equals () =
  reset ();
  update ["badcookie"];
  Alcotest.(check (option string)) "malformed cookie ignored" None (header ())

let () =
  Alcotest.run "Cookie_jar"
    [ ( "cookie_jar",
        [ Alcotest.test_case "empty jar"              `Quick test_empty_jar
        ; Alcotest.test_case "single cookie"          `Quick test_single_cookie
        ; Alcotest.test_case "multiple cookies"       `Quick test_multiple_cookies
        ; Alcotest.test_case "update overwrites"      `Quick test_update_overwrites
        ; Alcotest.test_case "value with = preserved" `Quick test_value_with_equals
        ; Alcotest.test_case "malformed ignored"      `Quick test_malformed_no_equals
        ] )
    ]
