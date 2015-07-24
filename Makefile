
OCAMLBUILD ?= ocamlbuild
OPTS ?= -use-ocamlfind
TARGETS = ty.cmxa ty.cma ty.cmxs

all:
	$(OCAMLBUILD) $(OPTS) $(TARGETS)

clean:
	$(OCAMLBUILD) -clean

.PHONY: all clean
