open Kwk_monitor_lib.Cookie_jar

let test_empty_jar () =
  let jar = create () in
  Alcotest.(check (option string)) "empty jar returns None" None (header jar)

let test_single_cookie () =
  let jar = create () in
  update jar ["__cf_bm=abc123; Path=/; HttpOnly"];
  match header jar with
  | None   -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "cookie header" "__cf_bm=abc123" h

let test_multiple_cookies () =
  let jar = create () in
  update jar ["cart=xyz; Path=/"; "session=tok; HttpOnly"];
  match header jar with
  | None   -> Alcotest.fail "expected Some"
  | Some h ->
    let parts = String.split_on_char ';' h |> List.map String.trim in
    Alcotest.(check int) "two cookies in header" 2 (List.length parts)

let test_update_overwrites () =
  let jar = create () in
  update jar ["__cf_bm=first; Path=/"];
  update jar ["__cf_bm=second; Path=/"];
  match header jar with
  | None   -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "value overwritten" "__cf_bm=second" h

let test_value_with_equals () =
  let jar = create () in
  update jar ["token=ab==cd; Path=/"];
  match header jar with
  | None   -> Alcotest.fail "expected Some"
  | Some h -> Alcotest.(check string) "value preserves =" "token=ab==cd" h

let test_malformed_no_equals () =
  let jar = create () in
  update jar ["badcookie"];
  Alcotest.(check (option string)) "malformed cookie ignored" None (header jar)

let test_independent_jars () =
  let jar1 = create () in
  let jar2 = create () in
  update jar1 ["a=1"];
  update jar2 ["b=2"];
  Alcotest.(check (option string)) "jar1 has a" (Some "a=1") (header jar1);
  Alcotest.(check (option string)) "jar2 has b" (Some "b=2") (header jar2)

let () =
  Alcotest.run "Cookie_jar"
    [ ( "cookie_jar",
        [ Alcotest.test_case "empty jar"              `Quick test_empty_jar
        ; Alcotest.test_case "single cookie"          `Quick test_single_cookie
        ; Alcotest.test_case "multiple cookies"       `Quick test_multiple_cookies
        ; Alcotest.test_case "update overwrites"      `Quick test_update_overwrites
        ; Alcotest.test_case "value with = preserved" `Quick test_value_with_equals
        ; Alcotest.test_case "malformed ignored"      `Quick test_malformed_no_equals
        ; Alcotest.test_case "jars are independent"   `Quick test_independent_jars
        ] )
    ]
