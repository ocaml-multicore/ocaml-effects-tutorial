set -uex

sh .travis-ocaml.sh
export OPAMYES=1
eval $(opam config env)

opam install ocamlfind ocamlbuild
make -C sources
