OCAMLBUILD=ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
                                     -ocamlopt '-toolchain metaocaml ocamlopt'

all: letrec.cma letrec.cmxa ppx_letrec.byte

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

.PHONY: all clean test
