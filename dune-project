(lang dune 3.19)

(name occ)

(generate_opam_files true)

(source
 (github hasanpasha/occ))

(authors "Hasan Pasha <bashahsn22@gmail.com>")

; (maintainers "Maintainer Name <maintainer@example.com>")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name occ)
 (synopsis "C compiler")
 (description "A C programming language compiler")
 (depends ocaml cmdliner ppx_deriving)
 (tags
  ("C" "programming" "language" "compiler" your project)))