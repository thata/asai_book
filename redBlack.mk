all: redBlackTest

clean:
	rm -f *.cmo *.cmi *.top redBlackTest

%.cmi: %.mli
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

redBlackTest: redBlack.cmi redBlack.cmo redBlackTest.cmo
	ocamlc -o redBlackTest redBlack.cmo redBlackTest.cmo

run: redBlackTest
	./redBlackTest

redBlack.top:
	ocamlmktop -o redBlack.top redBlack.mli redBlack.ml

top: redBlack.top
	rlwrap ./redBlack.top
