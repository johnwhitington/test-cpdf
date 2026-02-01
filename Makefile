# Build the cpdf command line tools and top level
SOURCES = cpdftest.ml

RESULT = cpdftest

PACKS = unix camlpdf

OCAMLNCFLAGS = -g
OCAMLBCFLAGS = -g
OCAMLLDFLAGS = -g

all : native-code

-include OCamlMakefile

