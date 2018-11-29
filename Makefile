.PHONY : all
all:
	dune build ocamlcodoc

.PHONY : install
install :
	dune build ocamlcodoc @install
	dune install
