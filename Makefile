OCAMLOPT ?= ocamlopt
ocamlfind := ocamlfind $(OCAMLOPT) -I .

compilation_extensions := o,cma,cmo,cmi,cmx,cmxa,cmxo,cmxi
interface_extensions := mli,

source := types.ml grammars.ml pwz.ml

.PHONY: clean
.PHONY: clean-all
.PHONY: compile
.PHONY: default
.PHONY: interfaces

default: compile

clean:
	$(RM) *.{$(compilation_extensions)}

clean-all: clean
	$(RM) *.{$(interface_extensions)}
	$(RM) pwz

pwz: types.ml pwz.ml
	$(ocamlfind) -o $@ $^

compile: types.ml pwz.ml grammars.ml
	$(ocamlfind) -c $^

interfaces: $(patsubst %.ml, %.mli, $(sources))

pwz.mli: pwz.ml types.cmi
	$(ocamlfind) -i $< > $@

%.cmi: %.mli
	$(ocamlfind) -c $^

%.mli: %.ml
	$(ocamlfind) -i $^ > $@
