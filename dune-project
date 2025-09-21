(lang dune 3.19)

(name occ)

(generate_opam_files true)

(source
 (github hasanpasha/occ))

(authors "Hasan Pasha <bashahsn22@gmail.com>")

(maintainers "Hasan Pasha <bashahsn22@gmail.com>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name occ)
 (synopsis "C compiler")
 (description "A C programming language compiler")
 (depends ocaml cmdliner logs fmt ppx_deriving ppx_string)
 (tags
  ("C" "programming" "language" "compiler" your project)))
