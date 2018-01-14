OCAMLC     = ocamlfind ocamlc
OCAMLOPT   = ocamlfind ocamlopt
OCAMLDEP   = ocamlfind ocamldep
WARNS      = Ael-3-31-40-41-42-44-45-48
override OCAMLOPTFLAGS += -w $(WARNS) -g -annot -O2
override OCAMLFLAGS    += -w $(WARNS) -g -annot 

.SUFFIXES: .ml .mli .cmo .cmi .cmx .cmxs .annot .opt .byte .html .adoc
.PHONY: clean distclean all check dep install deb doc

SOURCES = $(wildcard *.ml)

all: oneuneu

doc: README.html

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

%.html: %.adoc
	asciidoc -a data-uri -a icons -a max-width=55em --theme volnitsky -o $@ $<

clean:
	$(RM) *.cmo *.cmi .depend *.annot *.s *.cma *.cmxa
	$(RM) ONeuNeuConfig.*
	$(RM) -r debtmp

distclean: clean
	$(RM) *.cma *.o *.cmx *.cmxa *.cmxs *.cmi *.opt *.byte *.a *deb oneuneu

# Installation

bin_dir ?= /usr/bin
lib_dir ?= /var/lib/oneuneu

ONeuNeuConfig.ml:
	echo 'let lib_dir = "$(lib_dir)"' > $@

install: oneuneu vera.ttf
	strip oneuneu
	install -d $(DESTDIR)$(bin_dir)
	install oneuneu $(DESTDIR)$(bin_dir)
	install -d $(DESTDIR)$(lib_dir)
	install vera.ttf $(DESTDIR)$(lib_dir)

uninstall:
	$(RM) $(DESTDIR)$(bin_dir)/oneuneu
	$(RM) $(DESTDIR)$(lib_dir)/vera.ttf
	rmdir $(DESTDIR)$(lib_dir) || true

reinstall: uninstall install

# Debian

deb:
	mkdir -p debtmp
	$(MAKE) DESTDIR=debtmp bin_dir=$(bin_dir) lib_dir=$(lib_dir) install
	chmod a+x debtmp$(bin_dir)/oneuneu
	sudo chown root: debtmp$(bin_dir)/oneuneu
	$(RM) -r debtmp/DEBIAN
	mkdir debtmp/DEBIAN
	cp debian.control debtmp/DEBIAN/control
	dpkg --build debtmp
	mv debtmp.deb oneuneu.0.1.deb

# Dependencies

dep:
	$(RM) .depend
	$(MAKE) .depend

.depend: $(SOURCES)
	@$(OCAMLDEP) -package "$(PACKAGES)" $(filter %.ml, $(SOURCES)) $(filter %.mli, $(SOURCES)) > $@

include .depend
