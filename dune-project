(lang dune 3.14)

(name bc-monitor)

(generate_opam_files true)

(source (github iviviv/kwk-monitor))

(license MIT)

(package
 (name bc-monitor)
 (allow_empty)
 (synopsis "Configurable Big Cartel store monitor with Discord notifications")
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
