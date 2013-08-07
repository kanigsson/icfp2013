.PHONY: force

all: force
	ocamlbuild -use-ocamlfind main.native

clean:
	ocamlbuild -clean
cleanall: clean
	rm -f *~ */*~
