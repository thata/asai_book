all: tree_test

clean:
	rm -f *.cmo *.cmi *.top tree_test

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

tree_test: tree.cmi tree.cmo tree_test.cmo
	ocamlc -o tree_test tree.cmo tree_test.cmo

run: tree_test
	./tree_test
