(lang dune 3.23)
(name bf_caml)
(generate_opam_files)

(package
 (name bf_caml)
 (allow_empty)
 (depends
  (ocaml (and (>= 5.0) (< 6)))
  (hardcaml (= v0.17.1))
  (ppx_hardcaml (= v0.17.1))
  linenoise))
