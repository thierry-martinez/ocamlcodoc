(lang dune 1.11)

(generate_opam_files true)

(name ocamlcodoc)
(license BSD)
(maintainers "Thierry Martinez <thierry.martinez@inria.fr>")
(authors "Thierry Martinez <thierry.martinez@inria.fr>")
(source (uri "git+https://github.com/thierry.martinez/ocamlcodoc.git"))
(homepage "https://github.com/thierry.martinez/ocamlcodoc")
(bug_reports "https://github.com/thierry.martinez/ocamlcodoc")
(documentation "https://github.com/thierry.martinez/ocamlcodoc")

(package
 (name ocamlcodoc)
 (synopsis "Extract test code from doc-comments")
 (description "\
ocamlcodoc extracts the preformatted source code in OCaml
documentation comments, i.e. the code delimited by {[ ... ]} in
comments delimited by (** ... *). A typical usage is to write examples
in documentation comments that can be extracted and tested.
")
 (depends
  (ocaml (and (>= 4.03.0) (< 4.12.0)))
  (dune (>= 1.11.3))
  (cmdliner (>= 1.0.4))
  (redirect (>= 0.1.1))
  (stdcompat (>= 10))))
