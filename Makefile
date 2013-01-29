rank.native: rank.ml
	@ocamlbuild -use-ocamlfind -package z3 -package batteries $@

clean:
	@ocamlbuild -clean
