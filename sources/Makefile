EXE := input_line_exn.native input_line_exn2.native input_line_eff.native input_line_eff2.native \
			 exceptions.native state1.native state2.native \
			 gdb.native msg_passing.native fringe.native \
			 generator.native \
			 cooperative.native async_await.native \
			 echo_unix.native echo_async.native

all: $(EXE)

echo_unix.native: echo_unix.ml echo.ml
	ocamlbuild -pkg unix echo_unix.native

echo_async.native: echo.ml echo_async.ml
	ocamlbuild -pkg unix echo_async.native

%.native: %.ml
	ocamlbuild -cflags -g -lflags -g -cflags -w -cflags -8 $@

.PHONY: clean
clean:
	ocamlbuild -clean
	rm -f *~
