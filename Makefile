OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
                                     -ocamlopt '-toolchain metaocaml ocamlopt'

all: check-compiler letrec.cma letrec.cmxa ppx_letrec.native

test: tests.native
	./tests.native 

install:
	ocamlfind install letrec META \
          _build/lib/*.cma            \
          _build/lib/*.cmx            \
          _build/lib/*.cmxa           \
          _build/lib/*.a              \
          _build/lib/*.cmi            \
          _build/lib/*.mli            \
          _build/ppx/ppx_letrec.native  

uninstall:
	$(OCAMLBUILD) remove letrec

clean:
	$(OCAMLBUILD) -clean

%.cma:
	$(OCAMLBUILD) -use-ocamlfind $@

%.cmxa:
	$(OCAMLBUILD) -use-ocamlfind $@

%.native:
	$(OCAMLBUILD) -use-ocamlfind $@

%.byte:
	$(OCAMLBUILD) -use-ocamlfind $@

tests.native: letrec.cmxa lib_test/tests.ml

check-compiler:
	@test $$(opam switch  show) = "4.04.2+ber-multicore" \
	|| (echo 1>&2 "Please use OPAM switch 4.04.2+ber-multicore"; exit 1)

.PHONY: check-compiler all clean test
