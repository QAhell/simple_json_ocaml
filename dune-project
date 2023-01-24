(lang dune 3.6)

(name simple_json)

(generate_opam_files true)

(source
 (github QAhell/simple_json_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license "Apache-2.0")

(documentation https://github.com/QAhell/simple_json_ocaml)

(package
 (name simple_json)
 (synopsis "A simple JSON parser and printer written in OCaml")
 (description "A simple JSON parser and printer written in OCaml")
 (depends ocaml dune simple_parser_combinator)
 (tags (topics "json")))
