PROGRAM=scheme
SRCDIR=src
BUILDDIR=bin
EXAMPLESDIR=examples
EXAMPLES=$(wildcard $(EXAMPLESDIR)/*.scm)

all:
	mkdir -p $(BUILDDIR) 
	ocamlfind ocamlc -c -package sedlex -package gen $(SRCDIR)/$(PROGRAM).ml
	ocamlfind ocamlc -o $(BUILDDIR)/$(PROGRAM) -linkpkg -package sedlex -package gen $(SRCDIR)/$(PROGRAM).cmo

test: 
	examples/diff.sh $(EXAMPLES)

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj
