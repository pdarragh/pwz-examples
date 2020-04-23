OCAMLOPT ?= ocamlopt
ocamlfind := ocamlfind $(OCAMLOPT) -I .

compilation_extensions := o,cma,cmo,cmi,cmx,cmxa,cmxo,cmxi
interface_extensions := mli,

.PHONY: default
.PHONY: clean
.PHONY: interfaces

default: pwz

clean:
	$(RM) *.{$(compilation_extensions)}

clean-all: clean
	$(RM) *.{$(interface_extensions)}
	$(RM) pwz

pwz: types.ml pwz.ml
	$(ocamlfind) -o $@ $^

interfaces: pwz.cmi types.mli

pwz.mli: pwz.ml types.cmi
	$(ocamlfind) -i $< > $@

%.ml: %.cmi

%.cmi: %.mli
	$(ocamlfind) -c $^

%.mli: %.ml
	$(ocamlfind) -i $^ > $@
