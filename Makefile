all: check-compiler letrec.cma ppx_letrec.byte

OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
                                     -ocamlopt '-toolchain metaocaml ocamlopt'

test: tests.byte
	./tests.byte 

install:
	ocamlfind install letrec META \
          _build/lib/*.cma            \
          _build/lib/*.cmi            \
          _build/lib/*.mli            \
          _build/ppx/ppx_letrec.byte  

uninstall:
	$(OCAMLBUILD) remove letrec

clean:
	$(OCAMLBUILD) -clean

%.cma %.native %.byte:
	$(OCAMLBUILD) -use-ocamlfind $@

tests.byte: letrec.cma lib_test/tests.ml

check-compiler:
	@test $$(opam switch  show) = "4.04.0+BER" \
	|| (echo 1>&2 "Please use OPAM switch 4.04.0+BER"; exit 1)

.PHONY: check-compiler all clean test
