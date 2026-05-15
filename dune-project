(lang dune 3.14)

(name kwk-monitor)

(generate_opam_files true)

(source (github iviviv/kwk-monitor))

(license MIT)

(package
 (name kwk-monitor)
 (allow_empty)
 (synopsis "IVIVIV Big Cartel sale monitor with Discord notifications")
 (depends
  (ocaml (>= 5.2.0))
  eio
  eio_main
  piaf
  yojson
  ppx_deriving_yojson
  ppx_deriving
  logs
  fmt
  (dotenv (>= 0.0.3))
  cmdliner
  alcotest))
