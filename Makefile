PROGRAM=scheme
SRCDIR=src
BUILDDIR=bin
EXAMPLESDIR=examples
EXAMPLES=$(wildcard $(EXAMPLESDIR)/*.scm)

all:
	ocamlbuild -j 0 -r -pkg sedlex -use-ocamlfind scheme.native
	mv scheme.native scheme

test: 
	# examples/diff.sh $(EXAMPLES)
	ocamlbuild -j 0 -r -pkg alcotest -pkg sedlex -use-ocamlfind test.native
	./test.native

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj
