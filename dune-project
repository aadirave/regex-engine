(lang dune 3.15)

(name regex_engine)

(generate_opam_files true)

(source
 (github username/reponame))

(authors "Author Name")

(maintainers "Maintainer Name")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name regex_engine)
 (allow_empty)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune ocamlfind ocamllex menhir)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
