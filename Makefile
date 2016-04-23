PROGRAM=scheme
SRCDIR=src
BUILDDIR=bin
EXAMPLESDIR=examples
EXAMPLES=$(wildcard $(EXAMPLESDIR)/*.scm)

all:
	ocamlbuild -j 0 -r -pkg sedlex -use-ocamlfind src/scheme.native
	mv scheme.native scheme

test: 
	examples/diff.sh $(EXAMPLES)

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj
