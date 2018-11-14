ACLOCAL_AMFLAGS = -I m4

.DEFAULT_GOAL = all

MODULES = utils.ml lexer.ml ocamlcodoc.ml

mypkgdir=$(pkgbindir)

mypkg_SCRIPTS = ocamlcodoc

OCAMLFLAGS = -package cmdliner

OCAMLCFLAGS = $(OCAMLFLAGS)

OCAMLLDFLAGS = $(OCAMLFLAGS)

SUFFIXES = .mll .ml .mli .cmi .cmo .cmx .cmxa .cmxs

.mll.ml :
	$(OCAMLLEX) $<

.mli.cmi :
	$(OCAMLBEST) -c $(OCAMLCFLAGS) $<

.ml.cmo :
	$(OCAMLC) -c $(OCAMLCFLAGS) $<

.ml.cmx :
	$(OCAMLOPT) -c $(OCAMLCFLAGS) $<

.PHONY : depend
depend : .depend

.depend : $(MODULES) $(MODULES:.ml=.mli)
	$(OCAMLDEP) $^ >$@ || rm -f .depend

@INCLUDE_DEPEND@

CLEANFILES = *.cmi *.cmo *.cmx *.o *.a

ocamlcodoc$(EXEEXT) : $(MODULES:.ml=.$(CMOX))
	$(OCAMLBEST) -linkpkg $(OCAMLLDFLAGS) $^ -o $@
