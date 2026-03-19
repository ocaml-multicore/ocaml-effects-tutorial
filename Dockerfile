FROM ocaml/opam:ubuntu-lts-ocaml-5.4

COPY dune-project ./
COPY sources/dune ./sources/
COPY sources/*.ml ./sources/
COPY sources/solved/dune ./sources/solved/
COPY sources/solved/*.ml ./sources/solved/

RUN eval $(opam env) && dune build
