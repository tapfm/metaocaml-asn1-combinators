#
# Based on https://raw.githubusercontent.com/ocaml/ocamlbuild/master/examples/04-library/Makefile
#

.PHONY: all clean byte native profile debug lib sanity test

OCB_FLAGS = -use-ocamlfind -I src -I tests
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte lib

clean:
	$(OCB) -clean

lib:
	$(OCB) asn_oid.cma
	$(OCB) asn_oid.cmx
	$(OCB) asn_oid.cmxa
	$(OCB) asn.cma
	$(OCB) asn.cmx
	$(OCB) asn.cmxa

