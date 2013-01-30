SRC = Matrix.ml rank.ml
OCAMLBUILD = ocamlbuild
BUILDFLAGS = -use-ocamlfind -package z3 -package batteries
BUILDTYPE = native

MAIN = rank.$(BUILDTYPE)

all: $(MAIN)

$(MAIN) : $(SRC)
	$(OCAMLBUILD) $(BUILDFLAGS) $@

clean:
	$(OCAMLBUILD) -clean
