OCAMLBUILD=ocamlbuild -use-ocamlfind

all: check-compiler letrec.cma letrec.cmxa ppx_letrec.byte

test: tests.byte
	./tests.byte 

install:
	ocamlfind install letrec META \
          _build/lib/*.cma            \
          _build/lib/*.cmx            \
          _build/lib/*.cmxa           \
          _build/lib/*.a              \
          _build/lib/*.cmi            \
          _build/lib/*.mli            \
          _build/ppx/ppx_letrec.byte  

uninstall:
	ocamlfind remove letrec

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

tests.byte: letrec.cma lib_test/tests.ml

check-compiler:
	@test $$(opam switch  show) = "4.07.1+BER"  \
      || test $$(opam switch  show) = "4.04.0+BER"  \
      || (echo 1>&2 "Please use OPAM switch 4.04.0+BER or 4.07.1+BER"; exit 1)

.PHONY: check-compiler all clean test
