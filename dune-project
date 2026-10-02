(lang dune 3.14)
(name vesper)
(generate_opam_files true)
(license MIT)

(package
 (name vesper)
 (synopsis "The Vesper programming language")
 (description "A programming language built from scratch in OCaml.")
 (depends
  (ocaml (>= 5.0.0))
  (dune (>= 3.14))
  menhir
  (alcotest :with-test)
  (ocamlformat :with-dev-setup)))
