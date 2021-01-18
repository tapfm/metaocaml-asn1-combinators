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
	$(OCB) src/asn_core.cmx
	$(OCB) src/asn_combinators.cmx
	$(OCB) src/unstaged/asn_unstaged_core.cmx
	$(OCB) src/unstaged/asn_unstaged_prim.cmx
	$(OCB) src/unstaged/asn_unstaged_reader.cmx
	$(OCB) src/unstaged/asn_unstaged_writer.cmx
	$(OCB) src/unstaged/asn_unstaged.cmx
	$(OCB) src/staged/asn_staged_core.cmx
	$(OCB) src/staged/asn_staged_prim.cmx
	$(OCB) src/staged/asn_staged_reader.cmx
	$(OCB) src/staged/asn_staged.cmx
	$(OCB) src/asn.cmx
	$(OCB) src/asn.cmxa

test-native:
	$(OCB) tests/test.native
	$(OCB) tests/x509.native
