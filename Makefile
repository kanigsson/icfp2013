.PHONY: force

all: force
	ocamlbuild -use-ocamlfind main.native
