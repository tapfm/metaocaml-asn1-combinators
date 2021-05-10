#
# Based on: https://raw.githubusercontent.com/ocaml/ocamlbuild/master/examples/04-library/Makefile
# and:      https://raw.githubusercontent.com/yallop/ocaml-asp/master/Makefile
#

.PHONY: all clean lib test-native test bar

OCB = ocamlbuild -use-ocamlfind -ocamlc '-toolchain metaocaml ocamlc' \
						   		-ocamlopt '-toolchain metaocaml ocamlopt'

all : clean lib test x509_benchmark

clean:
	$(OCB) -clean
	rm -f tmp/*

lib:
	$(OCB) asn.cma asn.cmxa

test: lib
	$(OCB) tests/test.native
	$(OCB) tests/x509.native

x509_stager: lib
	ulimit -s unlimited
	$(OCB) benchmarks/x509/x509_stager.native
	./x509_stager.native

x509_benchmark: x509_stager
	$(OCB) benchmarks/x509/x509_benchmark.native
	./x509_benchmark.native

decode: lib
	$(OCB) benchmarks/x509/decode.native
	./decode.native

bool_stager: lib
	$(OCB) benchmarks/toy/bool/bool_stager.native
	./bool_stager.native

bool_benchmark: bool_stager
	$(OCB) benchmarks/toy/bool/bool_benchmarks.native
	./bool_benchmarks.native

intseq_stager: lib
	$(OCB) benchmarks/toy/intseq/intseq_stager.native
	./intseq_stager.native

intseq_benchmark: intseq_stager
	$(OCB) benchmarks/toy/intseq/intseq_benchmark.native
	./intseq_benchmark.native

hnis_stager: lib
	$(OCB) benchmarks/toy/hnis/hnis_stager.native
	./hnis_stager.native

hnis_benchmark: hnis_stager
	$(OCB) benchmarks/toy/hnis/hnis_benchmark.native
	./hnis_benchmark.native

kerberos_stager: lib
	ulimit -s unlimited
	$(OCB) benchmarks/kerberos/kerberos_stager.native
	./kerberos_stager.native

kerberos_benchmark:
	$(OCB) benchmarks/kerberos/kerberos_benchmark.native
	./kerberos_benchmark.native

ldap_stager: lib
	ulimit -s unlimited
	$(OCB) benchmarks/ldap/ldap_stager.native
	./ldap_stager.native

ldap_benchmark: ldap_stager
	$(OCB) benchmarks/ldap/ldap_benchmark.native
	./ldap_benchmark.native

snmp_stager: lib
	ulimit -s unlimited
	$(OCB) benchmarks/snmp/snmp_stager.native
	./snmp_stager.native

snmp_benchmark: snmp_stager
	$(OCB) benchmarks/snmp/snmp_benchmark.native
	./snmp_benchmark.native
