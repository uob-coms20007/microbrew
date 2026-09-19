(lang dune 3.19)

(name microbrew)

(generate_opam_files true)

(source
 (github coms20007/microbrew))

(authors "Steven Ramsay <steven.ramsay@bristol.ac.uk>")

(maintainers "Steven Ramsay <steven.ramsay@bristol.ac.uk>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name microbrew)
 (synopsis "The Little Bristol Rewriter")
 (description "A Tiny Term Rewriting System for University of Bristol's COMS20007")
 (depends ocaml ppx_inline_test)
 (tags
  ("term rewriting" "repl")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
