EXE := exceptions.native echo_async.native fringe.native async_await.native generator.native state2.native

all: $(EXE)

echo_async.native: echo.ml echo_async.ml
	ocamlbuild -pkg unix -cflags -g echo_async.native

%.native: %.ml
	ocamlbuild -cflags -g -cflags -w $@

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~
