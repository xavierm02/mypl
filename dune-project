(lang dune 3.8)

(name mypl)

(generate_opam_files true)

(source
 (github xavierm02/mypl))

(authors "Xavier MONTILLET")

(maintainers "Xavier MONTILLET")

(license MIT)

(package
 (name mypl)
 (depends ocaml dune))

(package
 (name mycore)
 (depends ocaml dune))

(package
 (name modular_calculi)
 (depends ocaml dune))
