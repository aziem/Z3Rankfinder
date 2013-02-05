SRC = Matrix.ml Rankfinder.ml
OCAMLBUILD = ocamlbuild
BUILDFLAGS = -use-ocamlfind -package z3 -package batteries
BUILDTYPE = native

MAIN = Rankfinder.$(BUILDTYPE)

all: $(MAIN)

$(MAIN) : $(SRC)
	$(OCAMLBUILD) $(BUILDFLAGS) $@

clean:
	$(OCAMLBUILD) -clean
