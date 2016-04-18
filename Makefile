SRCDIR=src/
BUILDDIR=bin/

all:
	mkdir -p bin/
	ocamlfind ocamlc -c -package sedlex -package gen $(SRCDIR)scheme.ml
	ocamlfind ocamlc -o $(BUILDDIR)scheme -linkpkg -package sedlex -package gen $(SRCDIR)scheme.cmo

clean:
	rm -rf *~ *.cm* *.a *.lib *.o *.obj
