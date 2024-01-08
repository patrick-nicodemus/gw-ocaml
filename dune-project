(lang dune 3.12)

(name gw_partial)

(generate_opam_files true)

(source
 (github CamaraLab/CAJAL))

(authors "Patrick Nicodemus")

(maintainers "Patrick Nicodemus")

(license "MIT license")

(documentation https://cajal.readthedocs.io/en/latest/)

(package
 (name gw_partial)
 (synopsis "A short synopsis")
 (description "A longer description")
 (depends ocaml dune owl)
 (tags
  (topics "cell morphology" bioinformatics)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
