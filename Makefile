#
# Based on: https://raw.githubusercontent.com/ocaml/ocamlbuild/master/examples/04-library/Makefile
# and:      https://raw.githubusercontent.com/yallop/ocaml-asp/master/Makefile
#

.PHONY: all clean lib test-native test foo bar

OCB = ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
						   		-ocamlopt '-toolchain metaocaml ocamlopt'

all : clean lib test-native benchmark foo

clean:
	$(OCB) -clean
	rm tmp/*

lib:
	$(OCB) asn.cma asn.cmxa

test-native: lib
	$(OCB) tests/test.native
	$(OCB) tests/x509.native

stager: lib
	$(OCB) benchmarks/x509/stager.native
	./stager.native

benchmark: stager
	$(OCB) benchmarks/x509/x509_benchmark.native
	./x509_benchmark.native

decode: lib
	$(OCB) benchmarks/x509/decode.native
	./decode.native
