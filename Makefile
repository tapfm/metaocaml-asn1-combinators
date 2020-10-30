#
# Based on https://raw.githubusercontent.com/ocaml/ocamlbuild/master/examples/04-library/Makefile
#

.PHONY: clean lib test-native

OCB_FLAGS = -use-ocamlfind
OCB = ocamlbuild $(OCB_FLAGS)

clean:
	$(OCB) -clean

lib:
	$(OCB) src/asn_oid.cmx
	$(OCB) src/asn.cmx
	$(OCB) src/asn.cmxa

test-native:
	$(OCB) tests/oid_test.native
