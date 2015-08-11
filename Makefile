
OCAMLBUILD ?= ocamlbuild
OPTS ?= -use-ocamlfind
TARGETS = ty.cmxa ty.cma ty.cmxs src/ppx/ppx_deriving_ty.cma src/ppx/ppx_deriving_ty.cmxa examples/test.native

all:
	$(OCAMLBUILD) $(OPTS) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
