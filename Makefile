OCAMLC     = OCAMLPATH=$(OCAMLPATH) ocamlfind ocamlc
OCAMLOPT   = OCAMLPATH=$(OCAMLPATH) ocamlfind ocamlopt
OCAMLDEP   = OCAMLPATH=$(OCAMLPATH) ocamlfind ocamldep
WARNS      = Ael-3-31-40-41-42-44-45-48
override OCAMLOPTFLAGS += -w $(WARNS) -g -annot -O2
override OCAMLFLAGS    += -w $(WARNS) -g -annot 

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot .opt .byte
.PHONY: clean distclean all check dep install

SOURCES = $(wildcard *.ml)

all: oneuneu

PACKAGES = batteries,ogli

oneuneu: ONeuNeuConfig.cmx oneuneu.cmx
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -linkpkg -o $@ $^

%.byte: %.cmo
	$(OCAMLC) $(OCAMLFLAGS) -package "$(PACKAGES)" -linkpkg -o $@ $^

%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -package "$(PACKAGES)" -c $<

%.cmx: %.ml
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -package "$(PACKAGES)" -c $<

%.annot: %.cmx

clean:
	$(RM) *.cmo *.o *.cmi .depend *.annot *.s *.cma *.cmxa
	$(RM) ONeuNeuConfig.ml

distclean: clean
	$(RM) *.cma *.cmx *.cmxa *.cmxs *.cmi *.opt *.byte *.a oneuneu

# Installation

bin_dir ?= ./
lib_dir ?= ./

ONeuNeuConfig.ml:
	echo 'let lib_dir = "$(lib_dir)"' > $@

install: oneuneu vera.ttf
	install oneuneu $(prefix)$(bin_dir)
	install -d $(prefix)$(lib_dir)
	install vera.ttf $(prefix)$(lib_dir)

uninstall:
	$(RM) $(prefix)$(bin_dir)/oneuneu
	$(RM) $(prefix)$(lib_dir)/vera.ttf
	rmdir $(prefix)$(lib_dir) || true

reinstall: uninstall install

# Dependencies

dep:
	$(RM) .depend
	$(MAKE) .depend

.depend: $(SOURCES)
	@$(OCAMLDEP) -package "$(PACKAGES)" $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@

include .depend
