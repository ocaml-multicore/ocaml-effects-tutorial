FROM ocaml/opam:ubuntu-lts-ocaml-5.0

RUN opam install ocamlbuild ocamlfind

COPY sources/Makefile ./sources/
COPY sources/*.ml ./sources/

COPY sources/solved/Makefile ./sources/solved/
COPY sources/solved/*.ml ./sources/solved/

WORKDIR sources

RUN sudo bash -c "eval $(opam env) make all"

WORKDIR solved

RUN sudo bash -c "eval $(opam env) make all"
