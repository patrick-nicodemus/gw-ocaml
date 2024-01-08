(lang dune 3.10)

(name unbalanced_gw)

(generate_opam_files true)

(source
 (github CamaraLab/CAJAL))

(authors "Patrick Nicodemus")

(maintainers "Patrick Nicodemus")

(license "MIT license")

(documentation https://cajal.readthedocs.io/en/latest/)

(package
 (name unbalanced_gw)
 (synopsis "Computes the unbalanced Gromov-Wasserstein distance between point clouds or metric measure apces")
 (description "This package implements algorithms from the paper 'Scaling algorithms for unbalanced optimal transport' by Chizat, Gabriel Peyre, Bernhard Schmitzer, and Francois-Xavier Vialard, and the paper 'The Unbalanced Gromov Wasserstein Distance: Conic Formulation and Relaxation' by Thibault Séjourné, François-Xavier Vialard and Gabriel Peyré.")
 (depends ocaml owl base npy ctypes ctypes.foreign)
 (tags
  (topics "cell morphology" bioinformatics)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
