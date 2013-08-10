.PHONY: force

all: force
	ocamlbuild -use-ocamlfind main.native

debug:
	ocamlbuild -use-ocamlfind \
		-tags annot,debug \
		main.byte

clean:
	ocamlbuild -clean
cleanall: clean
	rm -f *~ */*~
