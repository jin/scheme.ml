SRCDIR=src/
BUILDDIR=bin/
EXAMPLESDIR=examples/

all:
	mkdir -p bin/
	ocamlfind ocamlc -c -package sedlex -package gen $(SRCDIR)scheme.ml
	ocamlfind ocamlc -o $(BUILDDIR)scheme -linkpkg -package sedlex -package gen $(SRCDIR)scheme.cmo

test:
	$(BUILDDIR)scheme < $(EXAMPLESDIR)arithmetic.scm
	$(BUILDDIR)scheme < $(EXAMPLESDIR)number_comparisons.scm
	$(BUILDDIR)scheme < $(EXAMPLESDIR)boolean_comparisons.scm
	$(BUILDDIR)scheme < $(EXAMPLESDIR)if.scm
	$(BUILDDIR)scheme < $(EXAMPLESDIR)quoted_list.scm
	$(BUILDDIR)scheme < $(EXAMPLESDIR)list_operations.scm

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj
